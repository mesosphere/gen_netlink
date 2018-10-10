-module(gen_netlink_errors).
-on_load(on_load/0).
-export([error/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "gen_netlink_errors"), 0).

-spec(erl_error_code(ErrNo :: non_neg_integer()) -> file:posix()).
erl_error_code(_ErrNo) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

-spec(error(ErrNo :: non_neg_integer()) -> file:posix()).
error(ErrNo) ->
    try
        erl_error_code(ErrNo)
    catch error:badarg ->
        Bin = <<ErrNo:32/native-integer>>,
        <<NegErrNo:32/big-signed-integer>> = Bin,
        erl_error_code(-NegErrNo)
    end.

-ifdef(TEST).

error_test() ->
    ?assertEqual(enodev, gen_netlink_errors:error(3992977407)).

-endif.
