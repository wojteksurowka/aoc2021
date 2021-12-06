-module(day1).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "(\\d+)$", [integer]).

part1() ->
    Depths = input(),
    Pairs = lists:zip([hd(Depths) | Depths], Depths ++ [lists:last(Depths)]),
    length(lists:filter(fun ({L, R}) -> L < R end, Pairs)).

part2() ->
    Depths = input(),
    Length = length(Depths) - 2,
    Triplets = apply(fun lists:zip3/3, [lists:sublist(Depths, Start, Length) || Start <- lists:seq(1, 3)]),
    Sums = [A + B + C || {A, B, C} <- Triplets],
    Pairs = lists:zip([hd(Sums) | Sums], Sums ++ [lists:last(Sums)]),
    length(lists:filter(fun ({L, R}) -> L < R end, Pairs)).