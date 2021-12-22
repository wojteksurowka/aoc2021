-module(day22).
-export([part1/0, part2/0]).

input() ->
    lists:map(fun ({Command, X1, X2, Y1, Y2, Z1, Z2}) ->
        {Command, {{X1, X2}, {Y1, Y2}, {Z1, Z2}}}
    end, aoc_input:read(?MODULE, "^ *(on|off) +x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+) *$", [atom | lists:duplicate(6, integer)])).

part1() ->
    All = lists:foldl(fun execute/2, [], input()),
    WithoutInitArea = execute({off, {{-50, 50}, {-50, 50}, {-50, 50}}}, All),
    count(All) - count(WithoutInitArea).

part2() ->
    count(lists:foldl(fun execute/2, [], input())).

count(Areas) ->
    lists:foldl(fun ({{X1, X2}, {Y1, Y2}, {Z1, Z2}}, Acc) ->
        Acc + (X2 - X1 + 1) * (Y2 - Y1 + 1) * (Z2 - Z1 + 1)
    end, 0, Areas).

execute({on, NewArea}, Areas) ->
    lists:flatten([NewArea | [subtract(Area, NewArea) || Area <- Areas]]);
execute({off, NewArea}, Areas) ->
    lists:flatten([subtract(Area, NewArea) || Area <- Areas]).

subtract({{AX1, AX2}, {AY1, AY2}, {AZ1, AZ2}}, {{BX1, BX2}, {BY1, BY2}, {BZ1, BZ2}}) ->
    case {normalise({BX1, BX2}, {AX1, AX2}), normalise({BY1, BY2}, {AY1, AY2}), normalise({BZ1, BZ2}, {AZ1, AZ2})} of
        {{NBX1, NBX2}, {NBY1, NBY2}, {NBZ1, NBZ2}} ->
            All = [
                {{AX1, NBX1 - 1}, {AY1, AY2}, {AZ1, AZ2}},
                {{NBX2 + 1, AX2}, {AY1, AY2}, {AZ1, AZ2}},
                {{NBX1, NBX2}, {AY1, AY2}, {AZ1, NBZ1 - 1}},
                {{NBX1, NBX2}, {AY1, AY2}, {NBZ2 + 1, AZ2}},
                {{NBX1, NBX2}, {AY1, NBY1 - 1}, {NBZ1, NBZ2}},
                {{NBX1, NBX2}, {NBY2 + 1, AY2}, {NBZ1, NBZ2}}
            ],
            lists:filter(fun ({{X1, X2}, {Y1, Y2}, {Z1, Z2}}) ->
                X1 =< X2 andalso Y1 =< Y2 andalso Z1 =< Z2
            end, All);
        _ ->
            [{{AX1, AX2}, {AY1, AY2}, {AZ1, AZ2}}]
    end.

normalise({B1, B2}, {A1, A2}) ->
    case B1 =< A2 andalso B2 >= A1 of
        true -> {max(B1, A1), min(B2, A2)};
        false -> false
    end.