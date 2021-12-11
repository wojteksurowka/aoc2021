-module(day7).
-export([part1/0, part2/0]).

input() ->
    [binary_to_integer(B) || B <- binary:split(hd(aoc_input:read(?MODULE, "^ *(.+) *$", [binary])), <<",">>, [global, trim_all])].

part1() ->
    find_min(fun (Crab, Position) -> abs(Crab - Position) end).

part2() ->
    find_min(fun (Crab, Position) ->
        L = abs(Crab - Position),
        round((1 + L) / 2 * L)
    end).

find_min(Fun) ->
    Crabs = input(),
    AllPossible = lists:seq(lists:min(Crabs), lists:max(Crabs)),
    lists:min(lists:map(fun (Position) ->
        lists:sum(lists:map(fun (Crab) -> Fun(Crab, Position) end, Crabs))
    end, AllPossible)).