%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2016, Mesosphere, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2016 5:16 PM
%%%-------------------------------------------------------------------
-module(gen_netlink_client).
-author("sdhillon").

-behavior(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include_lib("procket/include/packet.hrl").
-include_lib("gen_netlink/include/netlink.hrl").

-record(state, {
    port                :: port(),
    fd                  :: non_neg_integer(),
    pid                 :: non_neg_integer(),
    seq = 0             :: non_neg_integer(),
    last_rq_from        :: undefined | gen_statem:from(),
    replies = []        :: list(),
    current_seq         :: undefined | non_neg_integer(),
    family
}).

-type state_data() :: #state{}.

-type state_name() :: idle | wait_for_responses.

%% API
-export([start_link/0, start_link/1, start_link/2, start_link/3, stop/1]).
-export([request/6, request/5, request/4, get_family/2]).
-export([rtnl_request/3, rtnl_request/4]).
-export([if_nametoindex/1, if_nametoindex/2]).

%% gen_statem behaviour
-export([init/1, terminate/3, code_change/4, callback_mode/0]).

%% State callbacks
-export([idle/3, wait_for_responses/3, wait_for_responses_rtnl/3]).

-record(request, {family, subsys, cmd, flags, msg}).
-record(rtnl_request, {type, flags, msg}).

-define(PROCKETBIN, filename:join(code:priv_dir(procket), procket)).

-type response_message() :: term().
-type response_messages() :: [response_message()].
-type response_error() :: {error, ErrorCode :: file:posix(), response_messages()}.

-type request_reply() :: {ok, response_messages()} | response_error().
%% family() is taken by some stuff in gen_netlink
-type family2() :: non_neg_integer() | {generic, Id :: non_neg_integer(), Name :: atom()}.

-spec(request(Pid :: pid(), Family :: family2(), Command :: atom(), Msg :: term()) -> request_reply()).
request(Pid, Family, Command, Msg) ->
    request(Pid, Family, Command, [], Msg).
request(Pid, Family, Command, Flags, Msg) ->
    request(Pid, Family, netlink, Command, Flags, Msg).
request(Pid, Family, SubSys, Command, Flags, Msg) ->
    gen_statem:call(Pid, #request{family = Family, subsys = SubSys, cmd = Command, flags = Flags, msg = Msg}, 5000).


-spec(rtnl_request(Pid :: pid(), Type :: atom(), Msg :: term()) -> request_reply()).
rtnl_request(Pid, Type, Msg) ->
    rtnl_request(Pid, Type, [], Msg).
rtnl_request(Pid, Type, Flags, Msg) ->
    gen_statem:call(Pid, #rtnl_request{type = Type, flags = Flags, msg = Msg}, 5000).

stop(Pid) ->
    gen_statem:stop(Pid).

-spec(get_family(Pid :: pid(), FamilyName :: string()) -> response_error() | {ok, family2()}).
get_family(Pid, FamilyName) ->
    Request = [{family_name, FamilyName}],
    case request(Pid, ?NETLINK_GENERIC, ctrl, #getfamily{request = Request}) of
        {error, Code, Msg} ->
            {error, Code, Msg};
        {ok, [#netlink{msg = #newfamily{request = Response}}]} ->
            {family_id, Id} = proplists:lookup(family_id, Response),
            Family = {generic, Id, family_name_to_friendly(FamilyName)},
            {ok, Family}
    end.

family_name_to_friendly("IPVS") ->
    ipvs.

-spec(if_nametoindex(string()) -> {ok, non_neg_integer()} | {error, term()}).
if_nametoindex(Ifname) ->
    Opts = get_opts(packet, ?ETH_P_IP, raw),
    if_nametoindex2(Ifname, Opts).

-spec(if_nametoindex(string(), string()) -> {ok, non_neg_integer()} | {error, term()}).
if_nametoindex(Ifname, Netns) ->
    Opts = get_opts(packet, ?ETH_P_IP, raw, Netns),
    if_nametoindex2(Ifname, Opts).

if_nametoindex2(Ifname, Opts) ->
    {ok, Fd} = procket:open(0, Opts),
    try packet:ifindex(Fd, Ifname) of
        Idx when is_integer(Idx) andalso Idx >= 0 ->
            {ok, Idx}
    catch
        Exception:Reason ->
            {error, {Exception, Reason}}
    after
        procket:close(Fd)
    end.

start_link() ->
    gen_statem:start_link(?MODULE, [?NETLINK_GENERIC], []).

start_link(FamilyID) ->
    gen_statem:start_link(?MODULE, [FamilyID], []).

start_link(netns, Netns) ->
    gen_statem:start_link(?MODULE, [{netns, ?NETLINK_GENERIC, Netns}], []).

start_link(netns, FamilyID, Netns) ->
    gen_statem:start_link(?MODULE, [{netns, FamilyID, Netns}], []).

-spec(init([FamilyID :: non_neg_integer() | {netns, FamilyID :: non_neg_integer(), Netns :: string()}]) ->
    {ok, state_name(), state_data()}).
init([{netns, FamilyID, Netns}]) ->
    Opts = get_opts(netlink, FamilyID, dgram, Netns),
    init2(Opts);
init([FamilyID]) ->
    Opts = get_opts(netlink, FamilyID, dgram),
    init2(Opts).

init2(Opts) ->
    Pid = list_to_integer(os:getpid()),
    {ok, Fd} = procket:open(0, Opts),
    Port = erlang:open_port({fd, Fd, Fd}, [binary]),
    spawn_watch(Port, Fd),
    FamilyID = proplists:get_value(protocol, Opts),
    State = #state{fd = Fd, port = Port, pid = Pid, family = FamilyID},
    {ok, idle, State}.

idle({call, From}, #rtnl_request{type = Type, flags = Flags0, msg = Msg},
    State0 = #state{port = Port, pid = Pid, seq = Seq0, family = Family}) ->
    Flags1 = lists:usort([request, ack] ++ Flags0),
    Seq1 = Seq0 + 1,
    NLMsg = #rtnetlink{type = Type, flags = Flags1, seq = Seq0, pid = Pid, msg = Msg},
    ?LOG_DEBUG("NLMSG: ~p ~p", [Port, NLMsg]),
    Out = netlink_codec:nl_enc(family_id(Family), NLMsg),
    ?LOG_DEBUG("NLMSG Encoded: ~p ~p", [Port, Out]),
    erlang:port_command(Port, Out),
    State1 = State0#state{seq = Seq1, last_rq_from = From, replies = [], current_seq = Seq0},
    {next_state, wait_for_responses_rtnl, State1};

idle({call, From}, #request{subsys = SubSys, family = Family, cmd = Command, flags = Flags0, msg = Msg},
        State0 = #state{port = Port, pid = Pid, seq = Seq0}) ->
    Flags1 = lists:usort([ack, request] ++ Flags0),
    Seq1 = Seq0 + 1,
    NLMsg = {SubSys, Command, Flags1, Seq0, Pid, Msg},
    ?LOG_DEBUG("NLMSG: ~p ~p", [Port, NLMsg]),
    Out = netlink_codec:nl_enc(family_id(Family), NLMsg),
    ?LOG_DEBUG("NLMSG Encoded: ~p ~p", [Port, Out]),
    erlang:port_command(Port, Out),
    State1 = State0#state{seq = Seq1, family = Family, last_rq_from = From, replies = [], current_seq = Seq0},
    {next_state, wait_for_responses, State1};

idle(info, {Port, {data, Data}}, #state{port = Port, family = Family}) ->
    case netlink_codec:nl_dec(family_name(Family), Data) of
        [#netlink{type=done}] -> keep_state_and_data;
        Reason -> {stop, Reason}
    end.

wait_for_responses(info, {Port, {data, Data}}, #state{port = Port, family = Family}) ->
    ?LOG_DEBUG("Response Data: ~p ~p", [Port, Data]),
    Decoded = netlink_codec:nl_dec(family_name(Family), Data),
    ?LOG_DEBUG("Decoded: ~p", [Decoded]),
    NextEvents = lists:map(fun(M) -> {next_event, internal, {nl_msg, M}} end, Decoded),
    {keep_state_and_data, NextEvents};

wait_for_responses(internal, {nl_msg, Msg = {_SubSys, Type, _Flags, CurrentSeq, _Pid, _Payload}},
        State0 = #state{current_seq = CurrentSeq}) when Type == done; Type ==  error ->
    do_reply(Msg, State0),
    State1 = State0#state{last_rq_from = undefined, replies = [], current_seq = undefined},
    {next_state, idle, State1};
wait_for_responses(internal, {nl_msg, Msg = {_SubSys, _Type, _Flags, CurrentSeq, _Pid, _Payload}},
        State0 = #state{current_seq = CurrentSeq, replies = Replies0}) ->
    State1 = State0#state{replies = [Msg|Replies0]},
    {keep_state, State1}.

% minuteman_netlink:wait_for_responses_rtnl(internal, {nl_msg,{netlink,error,[],0,2253731618,


wait_for_responses_rtnl(info, {Port, {data, Data}}, #state{port = Port, family = Family}) ->
    ?LOG_DEBUG("Response Data: ~p ~p", [Port, Data]),
    Decoded = netlink_codec:nl_dec(family_name(Family), Data),
    NextEvents0 = lists:map(fun(M) -> {next_event, internal, {nl_msg, M}} end, Decoded),
    ?LOG_DEBUG("Decoded: ~p", [Decoded]),
    {keep_state_and_data, NextEvents0};
wait_for_responses_rtnl(internal, {nl_msg, Msg = #rtnetlink{type = Type, seq = CurrentSeq}},
    State0 = #state{current_seq = CurrentSeq}) when Type == done; Type == error ->
    do_reply(Msg, State0),
    State1 = State0#state{last_rq_from = undefined, replies = [], current_seq = undefined},
    {next_state, idle, State1};
wait_for_responses_rtnl(internal, {nl_msg, Msg = #netlink{type = Type, seq = CurrentSeq}},
    State0 = #state{current_seq = CurrentSeq}) when Type == done; Type == error ->
    do_reply(Msg, State0),
    State1 = State0#state{last_rq_from = undefined, replies = [], current_seq = undefined},
    {next_state, idle, State1};
wait_for_responses_rtnl(internal, {nl_msg, Msg = #rtnetlink{seq = CurrentSeq}},
    State0 = #state{current_seq = CurrentSeq, replies = Replies0}) ->
    State1 = State0#state{replies = [Msg|Replies0]},
    {keep_state, State1}.


do_reply(_, #state{last_rq_from = {To, _Tag}}) when not is_pid(To) ->
    throw(inconsistent_state);
do_reply(#netlink{type = done}, #state{last_rq_from = From, replies = Replies0}) ->
    Replies1 = lists:reverse(Replies0),
    gen_statem:reply(From, {ok, Replies1});
do_reply(#netlink{type = error, msg = {_Error = 0, _Payload}}, #state{last_rq_from = From, replies = Replies0}) ->
    Replies1 = lists:reverse(Replies0),
    gen_statem:reply(From, {ok, Replies1});
do_reply(#netlink{type = error, msg = {Error, _Payload}}, #state{last_rq_from = From, replies = Replies0}) ->
    Replies1 = lists:reverse(Replies0),
    ErrorCode = gen_netlink_errors:error(Error),
    gen_statem:reply(From, {error, ErrorCode, Replies1});
do_reply(#rtnetlink{type = done}, #state{last_rq_from = From, replies = Replies0}) ->
    Replies1 = lists:reverse(Replies0),
    gen_statem:reply(From, {ok, Replies1});
do_reply(#rtnetlink{type = error, msg = {_Error = 0, _Payload}}, #state{last_rq_from = From, replies = Replies0}) ->
    Replies1 = lists:reverse(Replies0),
    gen_statem:reply(From, {ok, Replies1});
do_reply(#rtnetlink{type = error, msg = {Error, _Payload}}, #state{last_rq_from = From, replies = Replies0}) ->
    Replies1 = lists:reverse(Replies0),
    ErrorCode = gen_netlink_errors:error(Error),
    gen_statem:reply(From, {error, ErrorCode, Replies1}).

get_opts(Family, FamilyID, Type) ->
    [{family, Family},
     {protocol, FamilyID},
     {type, Type}].

get_opts(Family, FamilyID, Type, Netns) ->
    [{family, Family},
     {protocol, FamilyID},
     {type, Type},
     {progname, "nsenter -n" ++ Netns ++ " " ++ ?PROCKETBIN}].

%% Returns ID, if generic family
family_id({generic, Id, _Name}) ->
    Id;
family_id(Family) when is_integer(Family) ->
    Family.

family_name({generic, _Id, Name}) ->
    Name;
family_name(Family) ->
    Family.

terminate(Reason, State, Data) ->
    ?LOG_WARNING("Terminating, due to: ~p, in state: ~p, with state data: ~p", [Reason, State, Data]).

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.

spawn_watch(Port, Fd) ->
    spawn(fun() -> watch(Port, Fd) end).

watch(Port, Fd) ->
    MonitorRef = monitor(port, Port),
    watch_loop(Port, Fd, MonitorRef).

watch_loop(Port, Fd, MonitorRef) ->
    receive
        {_, MonitorRef, _, _, _} ->
            procket:close(Fd);
        _ ->
            watch_loop(Port, Fd, MonitorRef)
    end.

callback_mode() ->
    state_functions.
