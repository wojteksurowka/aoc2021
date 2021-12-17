-module(day16).
-export([part1/0]).

input() ->
    join(lists:map(fun (D) ->
        <<(list_to_integer([D], 16)):4>>
    end, hd(aoc_input:read(?MODULE, "^ *([0-9A-F]+) *$", [list])))).

part1() ->
    total_version(element(1, packets(input(), undefined, []))).

packets(Input, 0, Acc) ->
    {lists:reverse(Acc), Input};
packets(<<>>, _Count, Acc) ->
    {lists:reverse(Acc), <<>>};
packets(<<Ver:3, 4:3, T/bitstring>>, Count, Acc) ->
    {Packet, Rest} = literal(Ver, T, <<>>),
    packets(Rest, decrease(Count), [Packet | Acc]);
packets(<<Ver:3, Type:3, 0:1, BodyLen:15, T/bitstring>>, Count, Acc) ->
    <<SubPacketBits:BodyLen, Rest/bitstring>> = T,
    {SubPackets, _} = packets(<<SubPacketBits:BodyLen>>, undefined, []),
    packets(Rest, decrease(Count), [{Ver, operator, Type, SubPackets} | Acc]);
packets(<<Ver:3, Type:3, 1:1, SubPacketCount:11, T/bitstring>>, Count, Acc) ->
    {SubPackets, Rest} = packets(T, SubPacketCount, []),
    packets(Rest, decrease(Count), [{Ver, operator, Type, SubPackets} | Acc]);
packets(Input, _Count, Acc) ->
    Size = bit_size(Input),
    <<X:Size>> = Input,
    case X of
        0 -> {lists:reverse(Acc), <<>>}
    end.

literal(Ver, <<1:1, Part:4, Rest/bitstring>>, Acc) ->
    literal(Ver, Rest, <<Acc/bitstring, Part:4>>);
literal(Ver, <<0:1, Part:4, Rest/bitstring>>, Acc) ->
    Value = <<Acc/bitstring, Part:4>>,
    Size = bit_size(Value),
    <<Literal:Size>> = Value,
    {{Ver, literal, Literal}, Rest}.

total_version([]) ->
    0;
total_version([{Ver, literal, _Literal} | T]) ->
    Ver + total_version(T);
total_version([{Ver, operator, _Type, SubPackets} | T]) ->
    Ver + total_version(SubPackets) + total_version(T).

decrease(undefined) -> undefined;
decrease(Count) -> Count - 1.

join([]) -> <<>>;
join([Binary]) -> Binary;
join([Binary | T]) -> <<Binary/bitstring, (join(T))/bitstring>>.