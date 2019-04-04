-ifndef(NETLINK_SERVER).
-module(netlink_codec).
-author("sdhillon").

%% API
-export([nl_ct_dec/1, nl_rt_dec/1,
    nl_rt_enc/1, nl_ct_enc/1,
    nl_dec/2, nl_enc/2,
    linkinfo_enc/3, linkinfo_dec/3,
    rtnl_wilddump/2]).

-export([nft_decode/2, nft_encode/2]).

-export([family_to_int/1, family_to_atom/1, protocol_to_int/1, protocol_to_atom/1]).
-endif.

-include("gen_netlink.hrl").
-include("gen_netlink_shared.hrl").
-include("netlink.hrl").

-type genl_family() :: integer() | {term(), integer()}.

-define(IS_NEW(Type), (
    Type == new_service orelse
    Type == new_dest orelse
    Type == newservice orelse
    Type == newlink orelse
    Type == newaddr orelse
    Type == newroute orelse
    Type == newneigh orelse
    Type == newrule orelse
    Type == newqdisc orelse
    Type == newtclass orelse
    Type == newtfilter orelse
    Type == newaction orelse
    Type == newprefix orelse
    Type == newneightbl orelse
    Type == newnduseropt orelse
    Type == newaddrlabel orelse
    Type == newtable orelse
    Type == newchain orelse
    Type == newrule orelse
    Type == newset orelse
    Type == newsetelem)).

-define(IS_GET(Type), (Type == get orelse
    Type == getlink orelse
    Type == getaddr orelse
    Type == getroute orelse
    Type == getneigh orelse
    Type == getrule orelse
    Type == getqdisc orelse
    Type == gettclass orelse
    Type == gettfilter orelse
    Type == getaction orelse
    Type == getmulticast orelse
    Type == getanycast orelse
    Type == getneightbl orelse
    Type == getaddrlabel orelse
    Type == getdcb orelse
    Type == get_ctrzero orelse
    Type == gettable orelse
    Type == getchain orelse
    Type == getrule orelse
    Type == getset orelse
    Type == getsetelem orelse
    Type == getgen)).


-define(NLMSG_MIN_TYPE, 16#10).

%% netlink info
-define(NETLINK_ADD_MEMBERSHIP, 1).
-define(NETLINK_DROP_MEMBERSHIP, 2).
-define(NETLINK_PKTINFO, 3).
-define(NETLINK_BROADCAST_ERROR, 4).
-define(NETLINK_NO_ENOBUFS, 5).

-define(SOL_NETLINK, 270).

-define(NFNLGRP_NONE, 0).
-define(NFNLGRP_CONNTRACK_NEW, 1).
-define(NFNLGRP_CONNTRACK_UPDATE, 2).
-define(NFNLGRP_CONNTRACK_DESTROY, 3).
-define(NFNLGRP_CONNTRACK_EXP_NEW, 4).
-define(NFNLGRP_CONNTRACK_EXP_UPDATE, 5).
-define(NFNLGRP_CONNTRACK_EXP_DESTROY, 6).

-define(NFNL_MSG_BATCH_BEGIN, ?NLMSG_MIN_TYPE).
-define(NFNL_MSG_BATCH_END,   ?NFNL_MSG_BATCH_BEGIN + 1).

-define(RTNLGRP_NONE, 0).
-define(RTNLGRP_LINK, 1).
-define(RTNLGRP_NOTIFY, 2).
-define(RTNLGRP_NEIGH, 3).
-define(RTNLGRP_TC, 4).
-define(RTNLGRP_IPV4_IFADDR, 4).
-define(RTNLGRP_IPV4_MROUTE, 5).
-define(RTNLGRP_IPV4_ROUTE, 6).
-define(RTNLGRP_IPV4_RULE, 7).
-define(RTNLGRP_IPV6_IFADDR, 8).
-define(RTNLGRP_IPV6_MROUTE, 9).
-define(RTNLGRP_IPV6_ROUTE, 10).
-define(RTNLGRP_IPV6_IFINFO, 11).
-define(RTNLGRP_DECnet_IFADDR, 12).
-define(RTNLGRP_NOP2,13).
-define(RTNLGRP_DECnet_ROUTE, 14).
-define(RTNLGRP_DECnet_RULE, 15).
-define(RTNLGRP_NOP4, 16).
-define(RTNLGRP_IPV6_PREFIX, 17).
-define(RTNLGRP_IPV6_RULE, 18).
-define(RTNLGRP_ND_USEROPT, 19).
-define(RTNLGRP_PHONET_IFADDR, 20).
-define(RTNLGRP_PHONET_ROUTE, 21).

-define(NLMSG_NOOP, 1).
-define(NLMSG_ERROR, 2).
-define(NLMSG_DONE, 3).
-define(NLMSG_OVERRUN, 4).

-define(RTM_NEWLINK, 16).
-define(RTM_DELLINK, 17).
-define(RTM_GETLINK, 18).
-define(RTM_SETLINK, 19).
-define(RTM_NEWADDR, 20).
-define(RTM_DELADDR, 21).
-define(RTM_GETADDR, 22).
-define(RTM_NEWROUTE, 24).
-define(RTM_DELROUTE, 25).
-define(RTM_GETROUTE, 26).
-define(RTM_NEWNEIGH, 28).
-define(RTM_DELNEIGH, 29).
-define(RTM_GETNEIGH, 30).
-define(RTM_NEWRULE, 32).
-define(RTM_DELRULE, 33).
-define(RTM_GETRULE, 34).
-define(RTM_NEWQDISC, 36).
-define(RTM_DELQDISC, 37).
-define(RTM_GETQDISC, 38).
-define(RTM_NEWTCLASS, 40).
-define(RTM_DELTCLASS, 41).
-define(RTM_GETTCLASS, 42).
-define(RTM_NEWTFILTER, 44).
-define(RTM_DELTFILTER, 45).
-define(RTM_GETTFILTER, 46).
-define(RTM_NEWACTION, 48).
-define(RTM_DELACTION, 49).
-define(RTM_GETACTION, 50).
-define(RTM_NEWPREFIX, 52).
-define(RTM_GETMULTICAST, 58).
-define(RTM_GETANYCAST, 62).
-define(RTM_NEWNEIGHTBL, 64).
-define(RTM_GETNEIGHTBL, 66).
-define(RTM_SETNEIGHTBL, 67).
-define(RTM_NEWNDUSEROPT, 68).
-define(RTM_NEWADDRLABEL, 72).
-define(RTM_DELADDRLABEL, 73).
-define(RTM_GETADDRLABEL, 74).
-define(RTM_GETDCB, 78).
-define(RTM_SETDCB, 79).

-define(IPCTNL_MSG_CT_NEW, 0).
-define(IPCTNL_MSG_CT_GET, 1).
-define(IPCTNL_MSG_CT_DELETE, 2).
-define(IPCTNL_MSG_CT_GET_CTRZERO, 3).

-define(IPCTNL_MSG_EXP_NEW, 0).
-define(IPCTNL_MSG_EXP_GET, 1).
-define(IPCTNL_MSG_EXP_DELETE, 2).

-define(NFQNL_MSG_PACKET, 0).              %% packet from kernel to userspace
-define(NFQNL_MSG_VERDICT, 1).             %% verdict from userspace to kernel
-define(NFQNL_MSG_CONFIG, 2).              %% connect to a particular queue
-define(NFQNL_MSG_VERDICT_BATCH, 3).       %% batchv from userspace to kernel

-define(NFQA_CFG_UNSPEC, 0).
-define(NFQA_CFG_CMD, 1).                  %% nfqnl_msg_config_cmd
-define(NFQA_CFG_PARAMS, 2).               %% nfqnl_msg_config_params
-define(NFQA_CFG_QUEUE_MAXLEN, 3).         %% u_int32_t

-define(NFQNL_CFG_CMD_NONE, 0).
-define(NFQNL_CFG_CMD_BIND, 1).
-define(NFQNL_CFG_CMD_UNBIND, 2).
-define(NFQNL_CFG_CMD_PF_BIND, 3).
-define(NFQNL_CFG_CMD_PF_UNBIND, 4).

-define(NLA_F_NESTED, 16#8000).
-define(NLA_F_NET_BYTEORDER, 16#4000).

-include("netlink_decoder_gen.hrl").

dec_rtm_type(RtmType) ->
    decode_rtnetlink_rtm_type(RtmType).

dec_rtm_protocol(RtmProto) ->
    decode_rtnetlink_rtm_protocol(RtmProto).

dec_rtm_scope(RtmScope) ->
    decode_rtnetlink_rtm_scope(RtmScope).

dec_rtm_table(RtmTable) ->
    decode_rtnetlink_rtm_table(RtmTable).

%% decode_rtnetlink_link_protinfo(inet, Type, Value) ->
%%     decode_rtnetlink_link_protinfo_inet(inet, Type, Value);
decode_rtnetlink_link_protinfo(inet6, Type, Value) ->
    decode_rtnetlink_link_protinfo_inet6(inet6, Type, Value);
decode_rtnetlink_link_protinfo(Family, Type, Value) ->
    {decode_rtnetlink_link_protinfo, Family, Type, Value}.

decode_ctnetlink_protoinfo_dccp(Family, Type, Value) ->
    {decode_ctnetlink_protoinfo_dccp, Family, Type, Value}.
decode_ctnetlink_protoinfo_sctp(Family, Type, Value) ->
    {decode_ctnetlink_protoinfo_sctp, Family, Type, Value}.

decode_nl_msg_type(SubSys, Type) ->
    {SubSys, decode_nl_msg_type_1(SubSys, Type)}.

decode_nl_msg_type_1(nlmsg, Type) ->
    decode_nl_msgtype_nlmsg(Type);
decode_nl_msg_type_1(ctnetlink, Type) ->
    decode_nl_msgtype_ctnetlink(Type);
decode_nl_msg_type_1(ctnetlink_exp, Type) ->
    decode_nl_msgtype_ctnetlink_exp(Type);
decode_nl_msg_type_1(nftables, Type) ->
    decode_nl_msgtype_nftables(Type);
decode_nl_msg_type_1({netlink, netfilter}, Type) ->
    decode_nl_msgtype_nfnl(Type);
decode_nl_msg_type_1({netlink, generic}, Type) ->
    decode_nl_msgtype_generic(Type);
decode_nl_msg_type_1(nft_compat, Type) ->
    decode_nl_msgtype_nft_compat(Type);
decode_nl_msg_type_1(queue, Type) ->
    decode_nl_msgtype_queue(Type);
decode_nl_msg_type_1({netlink, gtp}, _Type) ->
    gtp;
decode_nl_msg_type_1({netlink, ipvs}, _Type) ->
    ipvs;
decode_nl_msg_type_1({netlink, tcp_metrics}, _Type) ->
    tcp_metrics.

decode_rtnetlink_rtm_flags(Flags) ->
    decode_flag(flag_info_rtnetlink_rtm_flags(), Flags).

decode_nlm_flags(Type, Flags) when ?IS_GET(Type) ->
    decode_flag(flag_info_nlm_get_flags(), Flags);

decode_nlm_flags(Type, Flags) when ?IS_NEW(Type) ->
    decode_flag(flag_info_nlm_new_flags(), Flags);

decode_nlm_flags(_Type, Flags) ->
    decode_flag(flag_info_nlm_flags(), Flags).

decode_iff_flags(Flags) ->
    decode_flag(flag_info_iff_flags(), Flags).

encode_iff_flags(Flags) ->
    encode_flag(flag_info_iff_flags(), Flags).

encode_rtnetlink_rtm_flags(Flags) ->
    encode_flag(flag_info_rtnetlink_rtm_flags(), Flags).

encode_rtnetlink_link_protinfo(inet6, Value) ->
    encode_rtnetlink_link_protinfo_inet6(inet6, Value);
encode_rtnetlink_link_protinfo(Family, Value) ->
    lager:error("encode_rtnetlink_link_protinfo: ~p~n", {Family, Value}).

encode_ctnetlink_protoinfo_dccp(Family, Value) ->
    lager:error("encode_ctnetlink: ~p~n", {Family, Value}).

encode_ctnetlink_protoinfo_sctp(Family, Value) ->
    lager:error("encode_ctnetlink_protoinfo_sctp: ~p~n", {Family, Value}).

encode_nl_msg(netfilter, netlink, Type) ->
    encode_nl_msgtype_nfnl(Type);
encode_nl_msg(generic, netlink, Type) ->
    encode_nl_msgtype_generic(Type);

encode_nl_msg(_Protocol, rtnetlink, Type) ->
    encode_rtm_msgtype_rtnetlink(Type);
encode_nl_msg(Protocol, netlink, ipvs) ->
    Protocol;
encode_nl_msg(Protocol, netlink, tcp_metrics) ->
    Protocol;
encode_nl_msg(Protocol, netlink, gtp) ->
    Protocol;

encode_nl_msg(_Protocol, nlmsg, Type) ->
    encode_nl_msgtype_nlmsg(Type);
encode_nl_msg(_Protocol, ctnetlink, Type) ->
    encode_nl_msgtype_ctnetlink(Type);
encode_nl_msg(_Protocol, ctnetlink_exp, Type) ->
    encode_nl_msgtype_ctnetlink_exp(Type);
%% encode_nl_msg(_Protocol, {nftables, netfilter}, Type) ->
%%     encode_nl_msgtype_nfnl(Type);
encode_nl_msg(_Protocol, nftables, Type) ->
    encode_nl_msgtype_nftables(Type);
encode_nl_msg(_Protocol, nft_compat, Type) ->
    encode_nl_msgtype_nft_compat(Type);
encode_nl_msg(_Protocol, queue, Type) ->
    encode_nl_msgtype_queue(Type).

encode_flag(_Type, [], Value) ->
    Value;
encode_flag(Type, [Flag|Next], Value) when is_integer(Flag) ->
    encode_flag(Type, Next, Value bor Flag);
encode_flag(Type, [Flag|Next], Value) when is_atom(Flag) ->
    case lists:keyfind(Flag, 2, Type) of
        {Pos, _} ->
            encode_flag(Type, Next, Value bor Pos);
        _ ->
            encode_flag(Type, Next, Value)
    end.

encode_flag(Type, Flag) ->
    lager:debug("encode_flag: ~p, ~p~n", [Type, Flag]),
    encode_flag(Type, Flag, 0).


decode_flag([], 0, Acc) ->
    Acc;
decode_flag([], Flag, Acc) ->
    [Flag|Acc];
decode_flag([{Pos, V}|Rest], Flag, Acc) ->
    if Pos band Flag /= 0 ->
        decode_flag(Rest, Flag bxor Pos, [V|Acc]);
        true ->
            decode_flag(Rest, Flag, Acc)
    end.

decode_flag(Type, Flag) ->
    decode_flag(Type, Flag, []).

enc_nla(NlaType, Data) ->
    pad_to(4, <<(size(Data)+4):16/native-integer, NlaType:16/native-integer, Data/binary>>).



encode_none(NlaType, Data) ->
    enc_nla(NlaType, Data).

encode_binary(NlaType, Data) ->
    enc_nla(NlaType, Data).

encode_string(NlaType, String) ->
    enc_nla(NlaType, <<(list_to_binary(String))/binary, 0>>).

encode_uint8(NlaType, Val) ->
    enc_nla(NlaType, <<Val:8>>).
encode_uint16(NlaType, Val) ->
    enc_nla(NlaType, <<Val:16>>).
encode_uint32(NlaType, Val) ->
    enc_nla(NlaType, <<Val:32>>).
encode_uint64(NlaType, Val) ->
    enc_nla(NlaType, <<Val:64>>).
encode_huint16(NlaType, Val) ->
    enc_nla(NlaType, <<Val:16/native-integer>>).
encode_huint32(NlaType, Val) ->
    enc_nla(NlaType, <<Val:32/native-integer>>).
encode_huint64(NlaType, Val) ->
    enc_nla(NlaType, <<Val:64/native-integer>>).

%% encode_int8(NlaType, Val) ->
%% 	enc_nla(NlaType, <<Val:8/signed-integer>>).
%% encode_int16(NlaType, Val) ->
%% 	enc_nla(NlaType, <<Val:16/signed-integer>>).
encode_int32(NlaType, Val) ->
    enc_nla(NlaType, <<Val:32/signed-integer>>).
%% encode_int64(NlaType, Val) ->
%% 	enc_nla(NlaType, <<Val:64/signed-integer >>).
%% encode_hint16(NlaType, Val) ->
%% 	enc_nla(NlaType, <<Val:16/native-signed-integer>>).
%% encode_hint32(NlaType, Val) ->
%% 	enc_nla(NlaType, <<Val:32/native-signed-integer>>).
%% encode_hint64(NlaType, Val) ->
%% 	enc_nla(NlaType, <<Val:64/native-signed-integer>>).

encode_protocol(NlaType, Proto) ->
    enc_nla(NlaType, <<(protocol(Proto)):8>>).
encode_mac(NlaType, {A, B, C, D, E, F}) ->
    enc_nla(NlaType, << A:8, B:8, C:8, D:8, E:8, F:8 >>);
encode_mac(NlaType, MAC) when is_binary(MAC), size(MAC) == 6 ->
    enc_nla(NlaType, MAC).

encode_addr(NlaType, {A, B, C, D}) ->
    enc_nla(NlaType, << A:8, B:8, C:8, D:8 >>);
encode_addr(NlaType, {A,B,C,D,E,F,G,H}) ->
    enc_nla(NlaType, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>).

encode_if_map(NlaType, {_Attr, MemStart, MemEnd, BaseAddr, Irq, Dma, Port}) ->
    %% WARNING: THIS might be broken, compiler specific aligment must be take into consideration
    enc_nla(NlaType, << MemStart:64/native-integer, MemEnd:64/native-integer,
        BaseAddr:64/native-integer, Irq:16/native-integer,
        Dma:8, Port:8, 0:32 >>).

encode_hsint32_array(NlaType, Req) ->
    enc_nla(NlaType, << <<H:4/native-signed-integer-unit:8>> || H <- tl(tuple_to_list(Req)) >>).

encode_huint32_array(NlaType, Req) ->
    enc_nla(NlaType, << <<H:4/native-integer-unit:8>> || H <- tl(tuple_to_list(Req)) >>).

encode_huint64_array(NlaType, Req) ->
    enc_nla(NlaType, << <<H:8/native-integer-unit:8>> || H <- tl(tuple_to_list(Req)) >>).

encode_nfqnl_cfg_msg({cmd, Command, Pf}) ->
    <<(encode_nfqnl_config_cmd(Command)):8, 0:8, (family(Pf)):16>>;
encode_nfqnl_cfg_msg({params, CopyRange, CopyMode}) ->
    << CopyRange:32, CopyMode:8 >>.

encode_nfqnl_attr({packet_hdr, PacketId, HwProtocol, Hook}) ->
    <<PacketId:32, HwProtocol:16, Hook:8>>;
encode_nfqnl_attr({verdict_hdr, Verdict, Id}) ->
    <<Verdict:32, Id:32>>;
encode_nfqnl_attr({timestamp, Sec, USec}) ->
    <<Sec:64, USec:64>>;
encode_nfqnl_attr({hwaddr, HwAddr}) ->
    <<(size(HwAddr)):16, 0:16, (pad_to(8, HwAddr))/binary>>;
encode_nfqnl_attr({_Type, Data}) when is_binary(Data) ->
    Data.

encode_genl_ctrl_attr_ops(Family, {Idx, Value}) ->
    enc_nla(Idx, nl_enc_nla(Family, fun encode_genl_ctrl_attr_op/2, Value)).

encode_genl_ctrl_attr_mcast_groups(Family, {Idx, Value}) ->
    enc_nla(Idx, nl_enc_nla(Family, fun encode_genl_ctrl_attr_mcast_grp/2, Value)).

encode_ipvs_service_attributes({flags, Flags, Mask}) ->
    <<Flags:32/native-integer, Mask:32/native-integer>>.
%%
%% decoder
%%

decode_binary(Val) ->
    Val.
decode_none(Val) ->
    Val.
decode_string(Val) ->
    binary_to_list(hd(binary:split(Val, <<0>>))).
decode_uint8(<< Val:8 >>) ->
    Val.
decode_uint16(<< Val:16 >>) ->
    Val.
decode_uint32(<< Val:32 >>) ->
    Val.
decode_uint64(<< Val:64 >>) ->
    Val.
decode_huint16(<< Val:16/native-integer, _/binary >>) ->
    Val.
decode_huint32(<< Val:32/native-integer >>) ->
    Val.
decode_huint64(<< Val:64/native-integer >>) ->
    Val.

%% decode_int8(<< Val:8/signed-integer >>) ->
%%     Val.
%% decode_int16(<< Val:16/signed-integer >>) ->
%%     Val.
decode_int32(<< Val:32/signed-integer >>) ->
    Val.
%% decode_int64(<< Val:64/signed-integer >>) ->
%%     Val.
%% decode_hint16(<< Val:16/native-signed-integer >>) ->
%%     Val.
%% decode_hint32(<< Val:32/native-signed-integer >>) ->
%%     Val.
%% decode_hint64(<< Val:64/native-signed-integer >>) ->
%%     Val.

decode_protocol(<< Proto:8 >>) ->
    protocol(Proto).
decode_mac(MAC) when size(MAC) == 6 ->
    MAC.
decode_addr(<< A:8, B:8, C:8, D:8 >>) ->
    {A, B, C, D};
decode_addr(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    {A,B,C,D,E,F,G,H}.

decode_if_map(Attr, << MemStart:64/native-integer, MemEnd:64/native-integer,
    BaseAddr:64/native-integer, Irq:16/native-integer,
    Dma:8, Port:8, _Pad/binary >>) ->
    %% WARNING: THIS might be broken, compiler specific aligment must be take into consideration
    {Attr, MemStart, MemEnd, BaseAddr, Irq, Dma, Port}.

decode_hsint32_array(Attr, Data) ->
    list_to_tuple([Attr | [ H || <<H:4/native-signed-integer-unit:8>> <= Data ]]).
%% decode_huint64_array(Attr, Data) ->
%%     list_to_tuple([Attr | [ H || <<H:8/native-signed-integer-unit:8>> <= Data ]]).

decode_huint32_array(Attr, Data) ->
    list_to_tuple([Attr | [ H || <<H:4/native-integer-unit:8>> <= Data ]]).
decode_huint64_array(Attr, Data) ->
    list_to_tuple([Attr | [ H || <<H:8/native-integer-unit:8>> <= Data ]]).

decode_nfqnl_cfg_msg(cmd, << Command:8, _Pad:8, Pf:16>>) ->
    {cmd, decode_nfqnl_config_cmd(Command), family(Pf)};
decode_nfqnl_cfg_msg(params, << CopyRange:32, CopyMode:8 >>) ->
    {params, CopyRange, CopyMode}.

decode_nfqnl_attr(packet_hdr, <<PacketId:32, HwProtocol:16, Hook:8>>) ->
    {packet_hdr, PacketId, HwProtocol, Hook};
decode_nfqnl_attr(verdict_hdr, <<Verdict:32, Id:32>>) ->
    {verdict_hdr, Verdict, Id};
decode_nfqnl_attr(timestamp, <<Sec:64, USec:64>>) ->
    {timestamp, Sec, USec};
decode_nfqnl_attr(hwaddr, <<Len:16, _Pad:16, HwAddr:Len/binary, _/binary>>) ->
    {hwaddr, HwAddr};
decode_nfqnl_attr(Type, Data) ->
    {Type, Data}.

decode_genl_ctrl_attr_ops(Family, Idx, Value) ->
    {Idx, nl_dec_nla(Family, fun decode_genl_ctrl_attr_op/3, Value)}.

decode_genl_ctrl_attr_mcast_groups(Family, Idx, Value) ->
    {Idx, nl_dec_nla(Family, fun decode_genl_ctrl_attr_mcast_grp/3, Value)}.

decode_ipvs_service_attributes(flags, <<Flags:32/native-integer, Mask:32/native-integer>>) ->
    {flags, Flags, Mask}.

%%
%% pad binary to specific length
%%   -> http://www.erlang.org/pipermail/erlang-questions/2008-December/040709.html
%%
pad_to(Width, Binary) ->
    case (Width - size(Binary) rem Width) rem Width of
        0 -> Binary;
        N -> <<Binary/binary, 0:(N*8)>>
    end.

pad_len(Block, Size) ->
    (Block - (Size rem Block)) rem Block.

nl_dec_nla(Family, Fun, << Len:16/native-integer, NlaType:16/native-integer, Rest/binary >> = RawNla, Acc) ->
    PayLoadLen = Len - 4,
    Padding = pad_len(4, PayLoadLen),
    {Next, NLA} =
        case Rest of
            << Data:PayLoadLen/bytes, _Pad:Padding/bytes, Next0/binary >> when is_function(Fun, 4) ->
                {Next0, Fun(Family, NlaType band 16#7FFF, Data, Acc)};

            << Data:PayLoadLen/bytes, _Pad:Padding/bytes, Next0/binary >> when is_function(Fun, 3) ->
                {Next0, Fun(Family, NlaType band 16#7FFF, Data)};

            Data when PayLoadLen == size(Data) andalso is_function(Fun, 4) ->
                %% NFQ does not allign the last NLA when it is NFQA_PAYLOAD
                %% accept unaligned attributes when they are the last one
                {<<>>, Fun(Family, NlaType band 16#7FFF, Data, Acc)};

            Data when PayLoadLen == size(Data) andalso is_function(Fun, 3) ->
                %% NFQ does not allign the last NLA when it is NFQA_PAYLOAD
                %% accept unaligned attributes when they are the last one
                {<<>>, Fun(Family, NlaType band 16#7FFF, Data)};

            _ ->
                lager:warning("nl_dec_nla: unable to decode pay load of ~p", [RawNla]),
                {<<>>, {rawdata, RawNla}}
        end,
    nl_dec_nla(Family, Fun, Next, [NLA | Acc]);
nl_dec_nla(_Family, _Fun, << >>, Acc) ->
    lists:reverse(Acc).

nl_dec_nla(Family, Fun, Data) ->
    nl_dec_nla(Family, Fun, Data, []).

nl_enc_nla(_Family, _Fun, [], _Context, Acc) ->
    list_to_binary(lists:reverse(Acc));
nl_enc_nla(Family, Fun, [Head|Rest], Context, Acc) when is_function(Fun, 2) ->
%    lager:debug("nl_enc_nla: ~w, ~w~n", [Family, Head]),
    H = Fun(Family, Head),
    nl_enc_nla(Family, Fun, Rest, Context, [H|Acc]);
nl_enc_nla(Family, Fun, [Head|Rest], Context, Acc) when is_function(Fun, 3) ->
%    lager:debug("nl_enc_nla: ~w, ~w~n", [Family, Head]),
    H = Fun(Family, Head, Context),
    nl_enc_nla(Family, Fun, Rest, [Head|Context], [H|Acc]).

nl_enc_nla(Family, Fun, Req)  ->
    nl_enc_nla(Family, Fun, Req, [], []).

nl_enc_payload(ctnetlink, _MsgType, {Family, Version, ResId, Req}) ->
    Fam = family(Family),
    Data = nl_enc_nla(Family, fun encode_ctnetlink/2, Req),
    << Fam:8, Version:8, ResId:16/native-integer, Data/binary >>;

nl_enc_payload(ctnetlink_exp, _MsgType, {Family, Version, ResId, Req}) ->
    Fam = family(Family),
    Data = nl_enc_nla(Family, fun encode_ctnetlink_exp/2, Req),
    << Fam:8, Version:8, ResId:16/native-integer, Data/binary >>;

nl_enc_payload(rtnetlink, MsgType, {Family, IfIndex, State, Flags, NdmType, Req})
    when MsgType == getneigh; MsgType == newneigh; MsgType == delneigh ->
    Fam = family(Family),
    Data = nl_enc_nla(Family, fun encode_rtnetlink_neigh/2, Req),
    << Fam:8, 0:8, 0:16, IfIndex:32/native-signed-integer, State:16/native-integer, Flags:8, NdmType:8, Data/binary >>;

nl_enc_payload(rtnetlink, MsgType, {Family, PrefixLen, Flags, Scope, Index, Req})
    when MsgType == newaddr; MsgType == deladdr; MsgType == getaddr ->
    Fam = family(Family),
    Data = nl_enc_nla(Family, fun encode_rtnetlink_addr/2, Req),
    << Fam:8, PrefixLen:8, Flags:8, Scope:8, Index:32/native-integer, Data/binary >>;

nl_enc_payload(rtnetlink, MsgType, {Family, DstLen, SrcLen, Tos, Table, Protocol, Scope, RtmType, Flags, Req})
    when MsgType == newroute; MsgType == delroute ; MsgType == getroute ->
    Fam = family(Family),
    lager:debug("nl_enc_payload: ~p~n", [{Family, DstLen, SrcLen, Tos, Table, Protocol, Scope, RtmType, Flags, Req}]),
    lager:debug("~p, ~p, ~p, ~p, ~p~n", [encode_rtnetlink_rtm_table(Table),
        encode_rtnetlink_rtm_protocol(Protocol),
        encode_rtnetlink_rtm_scope(Scope),
        encode_rtnetlink_rtm_type(RtmType),
        encode_rtnetlink_rtm_flags(Flags)]),

    Data = nl_enc_nla(Family, fun encode_rtnetlink_route/2, Req),
    << Fam:8, DstLen:8, SrcLen:8, Tos:8,
        (encode_rtnetlink_rtm_table(Table)):8,
        (encode_rtnetlink_rtm_protocol(Protocol)):8,
        (encode_rtnetlink_rtm_scope(Scope)):8,
        (encode_rtnetlink_rtm_type(RtmType)):8,
        (encode_rtnetlink_rtm_flags(Flags)):32/native-integer, Data/binary >>;

nl_enc_payload(rtnetlink, MsgType, {Family, DstLen, SrcLen, Tos, Table, Protocol, Scope, RtmType, Flags, Req})
    when MsgType == newrule; MsgType == delrule; MsgType == getrule->
    Fam = family(Family),
    lager:debug("nl_enc_payload: ~p~n", [{Family, DstLen, SrcLen, Tos, Table, Protocol, Scope, RtmType, Flags, Req}]),
    lager:debug("~p, ~p, ~p, ~p, ~p~n", [encode_rtnetlink_rtm_table(Table),
        encode_rtnetlink_rtm_protocol(Protocol),
        encode_rtnetlink_rtm_scope(Scope),
        encode_rtnetlink_rtm_type(RtmType),
        encode_rtnetlink_rtm_flags(Flags)]),

    Data = nl_enc_nla(Family, fun encode_rtnetlink_rule/2, Req),
    << Fam:8, DstLen:8, SrcLen:8, Tos:8,
        (encode_rtnetlink_rtm_table(Table)):8,
        (encode_rtnetlink_rtm_protocol(Protocol)):8,
        (encode_rtnetlink_rtm_scope(Scope)):8,
        (encode_rtnetlink_rtm_type(RtmType)):8,
        (encode_rtnetlink_rtm_flags(Flags)):32/native-integer, Data/binary >>;

nl_enc_payload(rtnetlink, MsgType, {Family, Type, Index, Flags, Change, Req})
    when MsgType == newlink; MsgType == dellink; MsgType == getlink ->
    Fam = family(Family),
    Type0 = arphdr(Type),
    Flags0 = encode_iff_flags(Flags),
    Change0 = encode_iff_flags(Change),
    Data = nl_enc_nla(Family, fun encode_rtnetlink_link/2, Req),
    <<Fam:8, 0:8, Type0:16/native-integer, Index:32/native-integer, Flags0:32/native-integer, Change0:32/native-integer, Data/binary >>;

nl_enc_payload(rtnetlink, MsgType,{Family, IfIndex, PfxType, PfxLen, Flags, Req})
    when MsgType == newprefix; MsgType == delprefix ->
    Fam = family(Family),
    Data = nl_enc_nla(Family, fun encode_rtnetlink_prefix/2, Req),
    << Fam:8, 0:8, 0:16, IfIndex:32/native-signed-integer, PfxType:8, PfxLen:8, Flags:8, 0:8, Data/binary >>;

nl_enc_payload(nftables, MsgType, {Family, Version, ResId, Req}) ->
    Fam = family(Family),
    Fun = case MsgType of
              _ when MsgType == newtable;   MsgType == gettable;   MsgType == deltable   -> fun encode_nft_table_attributes/2;
              _ when MsgType == newchain;   MsgType == getchain;   MsgType == delchain   -> fun encode_nft_chain_attributes/2;
              _ when MsgType == newrule;    MsgType == getrule;    MsgType == delrule    -> fun encode_nft_rule_attributes/2;
              _ when MsgType == newset;     MsgType == getset;     MsgType == delset     -> fun encode_nft_set_attributes/2;
              _ when MsgType == newsetelem; MsgType == getsetelem; MsgType == delsetelem -> fun encode_nft_set_elem_list_attributes/2;
              _ when MsgType == newgen;     MsgType == getgen                            -> fun encode_nft_gen_attributes/2
          end,
    Data = nl_enc_nla(Family, Fun, Req),
    << Fam:8, Version:8, ResId:16/native-integer, Data/binary >>;

nl_enc_payload(queue, MsgType, {Family, Version, ResId, Req}) ->
    Fam = family(Family),
    Fun = case MsgType of
              config -> fun encode_nfqnl_cfg_msg/2;
              _      -> fun encode_nfqnl_attr/2
          end,
    Data = nl_enc_nla(Family, Fun, Req),
    << Fam:8, Version:8, ResId:16/native-integer, Data/binary >>;

nl_enc_payload({netlink, generic}, _MsgType, {CtrlCmd, Version, ResId, Req}) ->
    Cmd = encode_genl_ctrl_cmd(CtrlCmd),
    Data = nl_enc_nla(CtrlCmd, fun encode_genl_ctrl_attr/2, Req),
    << Cmd:8, Version:8, ResId:16/native-integer, Data/binary >>;

nl_enc_payload({netlink, _GenlType}, gtp, {GtpCmd, Version, ResId, Req}) ->
    Cmd = encode_gtp_cmd(GtpCmd),
    Data = nl_enc_nla(GtpCmd, fun encode_gtp_attrs/2, Req),
    << Cmd:8, Version:8, ResId:16/native-integer, Data/binary >>;

nl_enc_payload({netlink, _GenlType}, ipvs, {IPVSCmd, Version, ResId, Req}) ->
    Cmd = encode_ipvs_cmd(IPVSCmd),
    Data = nl_enc_nla(IPVSCmd, fun encode_ipvs_attrs/2, Req),
    <<Cmd:8, Version:8, ResId:16/native-integer, Data/binary>>;

nl_enc_payload({netlink, _GenlType}, tcp_metrics, {TCPMCmd, Version, ResId, Req}) ->
    Cmd = encode_tcp_metrics_cmd(TCPMCmd),
    Data = nl_enc_nla(Cmd, fun encode_tcp_metrics_attrs/2, Req),
    <<Cmd:8, Version:8, ResId:16/native-integer, Data/binary>>;

%% Other
nl_enc_payload(_, _, Data)
    when is_binary(Data) ->
    Data.

nl_dec_payload(_Type, done, Type, <<Length:32/native-integer>>) when Type band ?NLA_F_NET_BYTEORDER =:= 0 ->
    Length;

nl_dec_payload(_Type, done, _Type, <<Length:32>>) ->
    Length;

%% Error
nl_dec_payload(_Type, error, Type, <<Error:32/native-signed-integer, Msg/binary>>) when Type band ?NLA_F_NET_BYTEORDER =:= 0 ->
    {erlang:abs(Error), Msg};

nl_dec_payload(_Type, error, _Type, <<Error:32, Msg/binary>>) ->
    {Error, Msg};

nl_dec_payload(ctnetlink, _MsgType, _Type, << Family:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    Fam = family(Family),
    { Fam, Version, ResId, nl_dec_nla(Fam, fun decode_ctnetlink/3, Data) };

nl_dec_payload(ctnetlink_exp, _MsgType, _Type, << Family:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    Fam = family(Family),
    { Fam, Version, ResId, nl_dec_nla(Fam, fun decode_ctnetlink_exp/3, Data) };

nl_dec_payload(rtnetlink, MsgType, _Type, << Family:8, _Pad1:8, _Pad2:16, IfIndex:32/native-signed-integer, State:16/native-integer, Flags:8, NdmType:8, Data/binary >>)
    when MsgType == getneigh; MsgType == newneigh; MsgType == delneigh ->
    Fam = family(Family),
    { Fam, IfIndex, State, Flags, NdmType, nl_dec_nla(Fam, fun decode_rtnetlink_neigh/3, Data) };

nl_dec_payload(rtnetlink, MsgType, _Type, << Family:8, DstLen:8, SrcLen:8, Tos:8, Table:8, Protocol:8, Scope:8, RtmType:8, Flags:32/native-integer, Data/binary >>)
    when MsgType == newroute; MsgType == delroute; MsgType == getroute ->
    Fam = family(Family),
    { Fam, DstLen, SrcLen, Tos, dec_rtm_table(Table), dec_rtm_protocol(Protocol), dec_rtm_scope(Scope), dec_rtm_type(RtmType), decode_rtnetlink_rtm_flags(Flags), nl_dec_nla(Fam, fun decode_rtnetlink_route/3, Data) };

nl_dec_payload(rtnetlink, MsgType, _Type, << Family:8, PrefixLen:8, Flags:8, Scope:8, Index:32/native-integer, Data/binary >>)
    when MsgType == newaddr; MsgType == deladdr ->
    Fam = family(Family),
    { Fam, PrefixLen, Flags, Scope, Index, nl_dec_nla(Fam, fun decode_rtnetlink_addr/3, Data) };

nl_dec_payload(rtnetlink, MsgType, _Type, << Family:8, _Pad:8, Type:16/native-integer, Index:32/native-integer, Flags:32/native-integer, Change:32/native-integer, Data/binary >>)
    when MsgType == newlink; MsgType == dellink; MsgType == getlink->
    Fam = family(Family),
    { Fam, arphdr(Type), Index, decode_iff_flags(Flags), decode_iff_flags(Change), nl_dec_nla(Fam, fun decode_rtnetlink_link/3, Data) };

nl_dec_payload(rtnetlink, MsgType, _Type, << Family:8, _Pad1:8, _Pad2:16, IfIndex:32/native-signed-integer, PfxType:8, PfxLen:8, Flags:8, _Pad3:8, Data/binary >>)
    when MsgType == newprefix; MsgType == delprefix ->
    Fam = family(Family),
    { Fam, IfIndex, PfxType, PfxLen, Flags, nl_dec_nla(Fam, fun decode_rtnetlink_prefix/3, Data) };
%% struct rtmsg
nl_dec_payload(rtnetlink, MsgType, _Type, << Family:8, DstLen:8, SrcLen:8, Tos:8, Table:8, Protocol:8, Scope:8, RtmType:8, Flags:32/native-integer, Data/binary >> )
    when MsgType == newrule; MsgType == delrule; MsgType == getrule ->
    Fam = family(Family),
    { Fam, DstLen, SrcLen, Tos, dec_rtm_table(Table), dec_rtm_protocol(Protocol), dec_rtm_scope(Scope), dec_rtm_type(RtmType), decode_rtnetlink_rtm_flags(Flags), nl_dec_nla(Fam, fun decode_rtnetlink_rule/3, Data) };

nl_dec_payload(nftables, MsgType, _Type, << Family:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    Fam = family(Family),
    Fun = case MsgType of
              _ when MsgType == newtable;   MsgType == gettable;   MsgType == deltable   -> fun decode_nft_table_attributes/3;
              _ when MsgType == newchain;   MsgType == getchain;   MsgType == delchain   -> fun decode_nft_chain_attributes/3;
              _ when MsgType == newrule;    MsgType == getrule;    MsgType == delrule    -> fun decode_nft_rule_attributes/3;
              _ when MsgType == newset;     MsgType == getset;     MsgType == delset     -> fun decode_nft_set_attributes/3;
              _ when MsgType == newsetelem; MsgType == getsetelem; MsgType == delsetelem -> fun decode_nft_set_elem_list_attributes/3;
              _ when MsgType == newgen;     MsgType == getgen                            -> fun decode_nft_gen_attributes/3
          end,
    { Fam, Version, ResId, nl_dec_nla(Fam, Fun, Data) };

nl_dec_payload(queue, MsgType, _Type, << Family:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    Fam = family(Family),
    Fun = case MsgType of
              config -> fun decode_nfqnl_cfg_msg/3;
              _      -> fun decode_nfqnl_attr/3
          end,
    { Fam, Version, ResId, nl_dec_nla(Fam, Fun, Data) };

nl_dec_payload({netlink, generic}, _MsgType, _Type, << Cmd:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    CtrlCmd = decode_genl_ctrl_cmd(Cmd),
    { CtrlCmd, Version, ResId, nl_dec_nla(CtrlCmd, fun decode_genl_ctrl_attr/3, Data) };

nl_dec_payload({netlink, gtp}, _MsgType, _Type, << Cmd:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    GtpCmd = decode_gtp_cmd(Cmd),
    { GtpCmd, Version, ResId, nl_dec_nla(GtpCmd, fun decode_gtp_attrs/3, Data) };

nl_dec_payload({netlink, ipvs}, _MsgType, _Type, << Cmd:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    IPVSCmd = decode_ipvs_cmd(Cmd),
    { IPVSCmd, Version, ResId, nl_dec_nla(IPVSCmd, fun decode_ipvs_attrs/3, Data) };

nl_dec_payload({netlink, tcp_metrics}, _MsgType, _Type, << Cmd:8, Version:8, ResId:16/native-integer, Data/binary >>) ->
    TCPMCmd = decode_tcp_metrics_cmd(Cmd),
    { TCPMCmd, Version, ResId, nl_dec_nla(TCPMCmd, fun decode_tcp_metrics_attrs/3, Data) };

nl_dec_payload(_SubSys, _MsgType, _Type, Data) ->
    io:format("unknown SubSys/MsgType: ~p/~p~n", [_SubSys, _MsgType]),
    lager:warning("unknown SubSys/MsgType: ~p/~p", [_SubSys, _MsgType]),
    Data.

nlmsg_ok(DataLen, MsgLen) ->
    (DataLen >= 16) andalso (MsgLen >= 16) andalso (MsgLen =< DataLen).

-spec nl_dec(genl_family(), binary()) -> [{'error',_} | #ctnetlink{} | #ctnetlink_exp{} | #rtnetlink{}].
nl_dec(?NETLINK_ROUTE, Msg) ->
    nl_rt_dec(?NETLINK_ROUTE, Msg, []);
nl_dec(Protocol, Msg) ->
    nl_ct_dec(Protocol, Msg, []).

-spec nl_ct_dec(binary()) -> [{'error',_} | #ctnetlink{} | #ctnetlink_exp{} | #rtnetlink{}].
nl_ct_dec(Msg) ->
    nl_ct_dec(?NETLINK_NETFILTER, Msg, []).

nl_ct_dec(Protocol, << Len:32/native-integer, Type:16/native-integer, Flags:16/native-integer, Seq:32/native-integer, Pid:32/native-integer, Data/binary >> = Msg, Acc) ->
    {DecodedMsg, Next} = case nlmsg_ok(size(Msg), Len) of
                             true ->
                                 PayLoadLen = Len - 16,
                                 << PayLoad:PayLoadLen/bytes, NextMsg/binary >> = Data,

                                 SubSys0 = decode_nl_subsys(Type bsr 8),
                                 {SubSys1, MsgType} =
                                     case {SubSys0, (Type band 16#00FF)} of
                                         {netlink, Reserved} when Reserved < 16#10 ->
                                             decode_nl_msg_type(nlmsg, Reserved);
                                         {netlink, Other} ->
                                             decode_nl_msg_type({SubSys0, decode_protocol_subsys(Protocol)}, Other);
                                         {_, Other} ->
                                             decode_nl_msg_type(SubSys0, Other)
                                     end,
                                 Flags0 = decode_nlm_flags(MsgType, Flags),
                                 {{ SubSys0, MsgType, Flags0, Seq, Pid, nl_dec_payload(SubSys1, MsgType, Type, PayLoad) }, NextMsg};
                             _ ->
                                 {{ error, format }, << >>}
                         end,
    nl_ct_dec(Protocol, Next, [DecodedMsg | Acc]);

nl_ct_dec(_Protocol, << >>, Acc) ->
    lists:reverse(Acc).

is_rt_dump(Type, Flags) ->
    (Type band 3) =:= 2 andalso Type =/= 2 andalso Flags band ?NLM_F_DUMP =/= 0.

-spec nl_rt_dec(binary()) -> [{'error',_} | #rtnetlink{}].
nl_rt_dec(Msg) ->
    nl_rt_dec(?NETLINK_ROUTE, Msg, []).

nl_rt_dec(Protocol, << Len:32/native-integer, Type:16/native-integer, Flags:16/native-integer, Seq:32/native-integer, Pid:32/native-integer, Data/binary >> = Msg, Acc) ->
    {DecodedMsg, Next} = case nlmsg_ok(size(Msg), Len) of
                             true ->
                                 PayLoadLen = Len - 16,
                                 << PayLoad:PayLoadLen/bytes, NextMsg/binary >> = Data,
                                 MsgType = decode_rtm_msgtype_rtnetlink(Type),
                                 MsgFlags = decode_nlm_flags(MsgType, Flags),
                                 RtMsg = #rtnetlink{type = MsgType,
                                     flags = MsgFlags,
                                     seq   = Seq,
                                     pid   = Pid},
                                 case is_rt_dump(Type, Flags) of
                                     true ->
                                         <<IfiFam:8, _Pad:8, _IfiType:16/native-integer, IfiIndex:32/native-integer, IfiFlags:32/native-integer, IfiChange:32/native-integer, Filter/binary >> = PayLoad,
                                         InfoMsg = #ifinfomsg{family = family(IfiFam),
                                             type = Type,
                                             index = IfiIndex,
                                             flags = IfiFlags,
                                             change = IfiChange},
                                         {RtMsg#rtnetlink{msg = [InfoMsg | nl_dec_nla(IfiFam, fun decode_rtnetlink_link/3, Filter)]}, NextMsg};

                                     _ ->
                                         {RtMsg#rtnetlink{msg = nl_dec_payload(rtnetlink, MsgType, Type, PayLoad)}, NextMsg}
                                 end;

                             _ ->
                                 {{ error, format }, << >>}
                         end,
    nl_rt_dec(Protocol, Next, [DecodedMsg | Acc]);

nl_rt_dec(_Protocol, << >>, Acc) ->
    lists:reverse(Acc).

linkinfo_dec(Family, "gtp", Data) ->
    nl_dec_nla(Family, fun decode_linkinfo_gtp/3, Data);
linkinfo_dec(_Family, _Kind, Data) ->
    Data.

linkinfo_enc(Family, "gtp", Data) ->
    nl_enc_nla(Family, fun encode_linkinfo_gtp/2, Data).

enc_nlmsghdr_flags(Type, Flags) when ?IS_GET(Type) ->
    encode_flag(flag_info_nlm_get_flags(), Flags);
enc_nlmsghdr_flags(Type, Flags) when ?IS_NEW(Type) ->
    encode_flag(flag_info_nlm_new_flags(), Flags);
enc_nlmsghdr_flags(_Type, Flags) ->
    encode_flag(flag_info_nlm_flags(), Flags).

encode_nl_subsys1(rtnetlink) ->
    encode_nl_subsys(netlink);
encode_nl_subsys1(SubSys) ->
    encode_nl_subsys(SubSys).

enc_nlmsghdr(Protocol, SubSys, MsgType, Flags, Seq, Pid, Req) when is_list(Flags) ->
    enc_nlmsghdr(Protocol, SubSys, MsgType, enc_nlmsghdr_flags(MsgType, Flags), Seq, Pid, Req);
enc_nlmsghdr(Protocol, SubSys, MsgType, Flags, Seq, Pid, Req) when is_integer(Flags), is_binary(Req) ->
    Payload = pad_to(4, Req),
    Len = 16 + byte_size(Payload),
    Type = (encode_nl_subsys1(SubSys) bsl 8) bor encode_nl_msg(decode_protocol_subsys(Protocol), SubSys, MsgType),
    << Len:32/native-integer, Type:16/native-integer, Flags:16/native-integer, Seq:32/native-integer, Pid:32/native-integer, Payload/binary >>.

nl_enc(?NETLINK_ROUTE, Msg) ->
    nl_rt_enc(?NETLINK_ROUTE, Msg);
nl_enc(Protocol, Msg) ->
    nl_ct_enc(Protocol, Msg).

nl_rt_enc(Msg) ->
    nl_rt_enc(?NETLINK_ROUTE, Msg).

nl_rt_enc(_Protocol, {rtnetlink, MsgType, Flags, Seq, Pid, PayLoad}) ->
    Data = nl_enc_payload(rtnetlink, MsgType, PayLoad),
    enc_nlmsghdr(?NETLINK_ROUTE, rtnetlink, MsgType, Flags, Seq, Pid, Data);

nl_rt_enc(Protocol, Msg)
    when is_list(Msg) ->
    nl_rt_enc(Protocol, Msg, []).

nl_rt_enc(_Protocol, [], Acc) ->
    list_to_binary(lists:reverse(Acc));
nl_rt_enc(Protocol, [Head|Rest], Acc) ->
    nl_rt_enc(Protocol, Rest, [nl_rt_enc(Protocol, Head)|Acc]).

nl_ct_enc(_Protocol, [], Acc) ->
    list_to_binary(lists:reverse(Acc));
nl_ct_enc(Protocol, [Head|Rest], Acc) ->
    nl_ct_enc(Protocol, Rest, [nl_ct_enc(Protocol, Head)|Acc]).

nl_ct_enc(Msg) ->
    nl_ct_enc(?NETLINK_NETFILTER, Msg).

nl_ct_enc(Protocol, Msg)
    when is_list(Msg) ->
    nl_ct_enc(Protocol, Msg, []);

nl_ct_enc(Protocol, {netlink, MsgType, Flags, Seq, Pid, PayLoad}) ->
    SubSys = {netlink, decode_protocol_subsys(Protocol)},
    Data = nl_enc_payload(SubSys, MsgType, PayLoad),
    enc_nlmsghdr(Protocol, netlink, MsgType, Flags, Seq, Pid, Data);

nl_ct_enc(Protocol, {SubSys, MsgType, Flags, Seq, Pid, PayLoad}) ->
    Data = nl_enc_payload(SubSys, MsgType, PayLoad),
    enc_nlmsghdr(Protocol, SubSys, MsgType, Flags, Seq, Pid, Data).

rtnl_wilddump(Family, Type) ->
    NumFamily = family(Family),
    enc_nlmsghdr(?NETLINK_ROUTE, rtnetlink, Type, [root, match, request], 0, 0, << NumFamily:8 >>).

nft_decode(Family, {expr, [{name, Name}, {data, Data}]}) ->
    {expr, [{name, Name}, {data, nft_decode(Family, Name, Data)}]}.

nft_decode(Family, "counter", Data) ->
    nl_dec_nla(Family, fun decode_nft_counter_attributes/3, Data);
nft_decode(Family, "immediate", Data) ->
    nl_dec_nla(Family, fun decode_nft_immediate_attributes/3, Data);
nft_decode(Family, "bitwise", Data) ->
    nl_dec_nla(Family, fun decode_nft_bitwise_attributes/3, Data);
nft_decode(Family, "lookup", Data) ->
    nl_dec_nla(Family, fun decode_nft_lookup_attributes/3, Data);
nft_decode(Family, "meta", Data) ->
    nl_dec_nla(Family, fun decode_nft_meta_attributes/3, Data);
nft_decode(Family, "payload", Data) ->
    nl_dec_nla(Family, fun decode_nft_payload_attributes/3, Data);
nft_decode(Family, "reject", Data) ->
    nl_dec_nla(Family, fun decode_nft_reject_attributes/3, Data);
nft_decode(Family, "ct", Data) ->
    nl_dec_nla(Family, fun decode_nft_ct_attributes/3, Data);
nft_decode(Family, "queue", Data) ->
    nl_dec_nla(Family, fun decode_nft_queue_attributes/3, Data);
nft_decode(Family, "cmp", Data) ->
    nl_dec_nla(Family, fun decode_nft_cmp_attributes/3, Data);
nft_decode(Family, "match", Data) ->
    nl_dec_nla(Family, fun decode_nft_match_attributes/3, Data);
nft_decode(Family, "target", Data) ->
    nl_dec_nla(Family, fun decode_nft_target_attributes/3, Data);
nft_decode(_Family, _Name, Data) ->
    Data.

nft_encode(Family, {expr, [{name, Name}, {data, NLA}]}) ->
    {expr, [{name, Name}, {data, nft_encode(Family, Name, NLA)}]}.

nft_encode(Family, "counter", NLA) ->
    nl_enc_nla(Family, fun encode_nft_counter_attributes/2, NLA);
nft_encode(Family, "immediate", NLA) ->
    nl_enc_nla(Family, fun encode_nft_immediate_attributes/2, NLA);
nft_encode(Family, "bitwise", NLA) ->
    nl_enc_nla(Family, fun encode_nft_bitwise_attributes/2, NLA);
nft_encode(Family, "lookup", NLA) ->
    nl_enc_nla(Family, fun encode_nft_lookup_attributes/2, NLA);
nft_encode(Family, "meta", NLA) ->
    nl_enc_nla(Family, fun encode_nft_meta_attributes/2, NLA);
nft_encode(Family, "payload", NLA) ->
    nl_enc_nla(Family, fun encode_nft_payload_attributes/2, NLA);
nft_encode(Family, "reject", NLA) ->
    nl_enc_nla(Family, fun encode_nft_reject_attributes/2, NLA);
nft_encode(Family, "ct", NLA) ->
    nl_enc_nla(Family, fun encode_nft_ct_attributes/2, NLA);
nft_encode(Family, "queue", NLA) ->
    nl_enc_nla(Family, fun encode_nft_queue_attributes/2, NLA);
nft_encode(Family, "cmp", NLA) ->
    nl_enc_nla(Family, fun encode_nft_cmp_attributes/2, NLA);
nft_encode(Family, "match", NLA) ->
    nl_enc_nla(Family, fun encode_nft_match_attributes/2, NLA);
nft_encode(Family, "target", NLA) ->
    nl_enc_nla(Family, fun encode_nft_target_attributes/2, NLA);
nft_encode(_Family, _Name, NLA) ->
    NLA.
