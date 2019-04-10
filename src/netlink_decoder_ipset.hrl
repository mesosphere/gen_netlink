%% ============================

decode_nl_msgtype_ipset(Id) ->
    Mapping = #{
        ?IPSET_CMD_NONE => none,
        ?IPSET_CMD_PROTOCOL => protocol,
        ?IPSET_CMD_CREATE => create,
        ?IPSET_CMD_DESTROY => destroy,
        ?IPSET_CMD_FLUSH => flush,
        ?IPSET_CMD_RENAME => rename,
        ?IPSET_CMD_SWAP => swap,
        ?IPSET_CMD_LIST => list,
        ?IPSET_CMD_SAVE => save,
        ?IPSET_CMD_ADD => add,
        ?IPSET_CMD_DEL => del,
        ?IPSET_CMD_TEST => test,
        ?IPSET_CMD_HEADER => header,
        ?IPSET_CMD_TYPE => type,
        ?IPSET_CMD_RESTORE => restore,
        ?IPSET_CMD_HELP => help,
        ?IPSET_CMD_VERSION => version,
        ?IPSET_CMD_QUIT => quit,
        ?IPSET_CMD_COMMIT => commit
    },
    maps:get(Id, Mapping, Id).

%% ============================

decode_ipset(Family, Id, Value, List) ->
    Type = Id band (bnot (?NLA_F_NET_BYTEORDER bor ?NLA_F_NESTED)),
    case lists:keyfind(Type, 1, List) of
        {Type, Key, none} ->
            {Key, decode_none(Value)};
        {Type, Key, binary} ->
            {Key, decode_binary(Value)};
        {Type, Key, string} ->
            {Key, decode_string(Value)};
        {Type, Key, protocol} ->
            {Key, decode_protocol(Value)};
        {Type, Key, uint8} ->
            {Key, decode_uint8(Value)};
        {Type, Key, uint16}
                when Id band ?NLA_F_NET_BYTEORDER > 0 ->
            {Key, decode_uint16(Value)};
        {Type, Key, uint16} ->
            {Key, decode_huint16(Value)};
        {Type, Key, uint32}
                when Id band ?NLA_F_NET_BYTEORDER > 0 ->
            {Key, decode_uint32(Value)};
        {Type, Key, uint32} ->
            {Key, decode_huint32(Value)};
        {Type, Key, uint64}
                when Id band ?NLA_F_NET_BYTEORDER > 0 ->
            {Key, decode_uint64(Value)};
        {Type, Key, uint64} ->
            {Key, decode_huint64(Value)};
        {Type, Key, Fun}
                when is_function(Fun, 1) ->
            {Key, Fun(Value)};
        {Type, Key, Fun}
                when is_function(Fun, 3) ->
            {Key, nl_dec_nla(Family, Fun, Value)};
        false ->
            {Type, Value}
    end.

%% ============================

decode_ipset_attrs(Family, MsgType, Id, Value) ->
    decode_ipset(Family, Id, Value, [
        {0, unspec, none},
        {1, protocol, uint8},
        {2, setname, string},
        {3, typename, string},
        {4, revision, uint8},
        {5, family, fun (V) -> family_to_atom(decode_uint8(V)) end},
        {6, flags, uint32}, % TODO: add ipset flags
        {7, data,
            case lists:member(MsgType, [create, list]) of
                true -> fun decode_ipset_attrs_cart_data/3;
                false -> fun decode_ipset_attrs_data/3
            end},
        {8, adt, fun decode_ipset_attrs_adt/3},
        {9, lineno, binary},
        {10, protocol_min, uint8},
        {11, index, uint16}
    ]).

%% ============================

decode_ipset_attrs_data(Family, Id, Value) ->
    decode_ipset(Family, Id, Value, [
        {0, unspec, none},
        {1, ip, fun decode_ipset_addr/3},
        {2, ip_to, fun decode_ipset_addr/3},
        {3, cidr, uint8},
        {4, port, uint16},
        {5, port_to, uint16},
        {6, timeout, uint32},
        {7, proto, protocol},
        {8, cadt_flags, uint32},
        {9, cadt_lineno, uint32},
        {10, mark, uint32},
        {11, markmask, uint32},
        {17, ether, none}, % TODO: l2addr
        {18, name, binary},
        {19, nameref, uint32},
        {20, ip2, fun decode_ipset_addr/3},
        {21, cidr2, uint8},
        {22, ip2_to, fun decode_ipset_addr/3},
        {23, iface, binary},
        {24, bytes, uint64},
        {25, packets, uint64},
        {26, comment, binary},
        {27, skbmark, binary}, % TODO: decode skbmark
        {28, skbprio, binary}, % TODO: decode skbprio
        {29, skbqueue, uint16}
    ]).

%% ============================

decode_ipset_attrs_cart_data(Family, Id, Value) ->
    decode_ipset(Family, Id, Value, [
        {0, unspec, none},
        {1, ip, fun decode_ipset_addr/3},
        {2, ip_to, fun decode_ipset_addr/3},
        {3, cidr, uint8},
        {4, port, uint16},
        {5, port_to, uint16},
        {6, timeout, uint32},
        {7, proto, uint8}, % TODO: parse proto
        {8, cadt_flags, uint32},
        {9, cadt_lineno, uint32},
        {10, mark, uint32},
        {11, markmask, uint32},
        {17, gc, binary},
        {18, hashsize, uint32},
        {19, maxelem, uint32},
        {20, netmask, binary},
        {21, probes, binary},
        {22, resize, binary},
        {23, size, uint32},
        {24, elements, uint32},
        {25, references, uint32},
        {26, memsize, uint32}
    ]).

%% ============================

decode_ipset_attrs_adt(Family, Id, Value) ->
    decode_ipset(Family, Id, Value, []).

%% ============================

decode_ipset_addr(Family, Id, Value) ->
    decode_ipset(Family, Id, Value, [
        {0, unspec, none},
        {1, inet, fun decode_addr/1},
        {2, inet6, fun decode_addr/1}
    ]).

%% ============================

encode_nl_msgtype_ipset(Value) ->
    Mapping = #{
        none => ?IPSET_CMD_NONE,
        protocol => ?IPSET_CMD_PROTOCOL,
        create => ?IPSET_CMD_CREATE,
        destroy => ?IPSET_CMD_DESTROY,
        flush => ?IPSET_CMD_FLUSH,
        rename => ?IPSET_CMD_RENAME,
        swap => ?IPSET_CMD_SWAP,
        list => ?IPSET_CMD_LIST,
        save => ?IPSET_CMD_SAVE,
        add => ?IPSET_CMD_ADD,
        del => ?IPSET_CMD_DEL,
        test => ?IPSET_CMD_TEST,
        header => ?IPSET_CMD_HEADER,
        type => ?IPSET_CMD_TYPE,
        restore => ?IPSET_CMD_RESTORE,
        help => ?IPSET_CMD_HELP,
        version => ?IPSET_CMD_VERSION,
        quit => ?IPSET_CMD_QUIT,
        commit => ?IPSET_CMD_COMMIT
    },
    maps:get(Value, Mapping, Value).

%% ============================

encode_ipset(Family, {Type, Value}, List) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Id, none} ->
            encode_none(Id, Value);
        {Type, Id, string} ->
            encode_string(Id, Value);
        {Type, Id, binary} ->
            encode_binary(Id, Value);
        {Type, Id, protocol} ->
            encode_protocol(Id, Value);
        {Type, Id, uint8} ->
            encode_uint8(Id, Value);
        {Type, Id, uint16} ->
            encode_uint16(Id bor ?NLA_F_NET_BYTEORDER, Value);
        {Type, Id, uint32} ->
            encode_uint32(Id bor ?NLA_F_NET_BYTEORDER, Value);
        {Type, Id, uint64} ->
            encode_uint64(Id bor ?NLA_F_NET_BYTEORDER, Value);
        {Type, Id, Fun}
                when is_function(Fun, 2) ->
            Fun(Family, {Id, Value});
        false when is_integer(Type), is_binary(Value) ->
            enc_nla(Type, Value)
    end.

%% ============================

encode_ipset_attrs(Family, MsgType, {Type, Value}) ->
    encode_ipset(Family, {Type, Value}, [
        {unspec, 0, none},
        {protocol, 1, uint8},
        {setname, 2, string},
        {typename, 3, string},
        {revision, 4, uint8},
        {family, 5, fun (_F, {I, V}) ->
            encode_uint8(I, family_to_int(V))
        end},
        {flags, 6, uint32}, % TODO: add ipset flags
        {data, 7, fun (F, {I, V}) ->
            Fun =
                case lists:member(MsgType, [create, list]) of
                    true -> fun encode_ipset_attrs_cart_data/2;
                    false -> fun encode_ipset_attrs_data/2
                end,
            enc_nla(I bor ?NLA_F_NESTED, nl_enc_nla(F, Fun, V))
        end},
        {adt, 8, fun (_F, {I, V}) ->
            Fun = fun encode_ipset_attrs_adt/2,
            enc_nla(I bor ?NLA_F_NESTED, nl_enc_nla(Family, Fun, V))
        end},
        {lineno, 9, binary},
        {protocol_min, 10, uint8},
        {index, 11, uint16}
    ]).

%% ============================

encode_ipset_attrs_data(Family, {Type, Value}) ->
    encode_ipset(Family, {Type, Value}, [
        {unspec, 0, none},
        {ip, 1, fun encode_ipset_ip/2},
        {ip_from, 1, fun encode_ipset_ip/2},
        {ip_to, 2, fun encode_ipset_ip/2},
        {cidr, 3, uint8},
        {port, 4, uint16},
        {port_from, 4, uint16},
        {port_to, 5, uint16},
        {timeout, 6, uint32},
        {proto, 7, protocol},
        {cadt_flags, 8, uint32},
        {cadt_lineno, 9, uint32},
        {mark, 10, uint32},
        {markmask, 11, uint32},
        {ether, 17, none}, % TODO: l2addr
        {name, 18, binary},
        {nameref, 19, uint32},
        {ip2, 20, fun encode_ipset_ip/2},
        {cidr2, 21, uint8},
        {ip2_to, 22, fun encode_ipset_ip/2},
        {iface, 23, binary},
        {bytes, 24, uint64},
        {packets, 25, uint64},
        {comment, 26, binary},
        {skbmark, 27, binary}, % TODO: encode skbmark
        {skbprio, 28, binary}, % TODO: encode skbprio
        {skbqueue, 29, uint16}
    ]).

%% ============================

encode_ipset_attrs_cart_data(Family, {Type, Value}) ->
    encode_ipset(Family, {Type, Value}, [
        {unspec, 0, none},
        {ip, 1, fun encode_ipset_ip/2},
        {ip_from, 1, fun encode_ipset_ip/2},
        {ip_to, 2, fun encode_ipset_ip/2},
        {cidr, 3, uint8},
        {port, 4, uint16},
        {port_from, 4, uint16},
        {port_to, 5, uint16},
        {timeout, 6, uint32},
        {proto, 7, uint8},
        {cadt_flags, 8, uint32},
        {cadt_lineno, 9, uint32},
        {mark, 10, uint32},
        {markmask, 11, uint32},
        {gc, 17, binary},
        {hashsize, 18, uint32},
        {maxelem, 19, uint32},
        {netmask, 20, binary},
        {probes, 21, binary},
        {resize, 22, binary},
        {size, 23, uint32},
        {elements, 24, uint32},
        {references, 25, uint32},
        {memsize, 26, uint32}
    ]).

%% ============================

encode_ipset_attrs_adt(Family, {Type, Value}) ->
    encode_ipset(Family, {Type, Value}, []).

%% ============================

encode_ipset_ip(Family, {Id, Value}) ->
    enc_nla(Id bor ?NLA_F_NESTED, nl_enc_nla(Family, fun encode_ipset_addr/2, Value)).

encode_ipset_addr(Family, {Type, Value}) ->
    Fun = fun (_F, {I, V}) -> encode_addr(I bor ?NLA_F_NET_BYTEORDER, V) end,
    encode_ipset(Family, {Type, Value}, [
        {unspec, 0, none},
        {inet, 1, Fun},
        {inet6, 2, Fun}
    ]).
