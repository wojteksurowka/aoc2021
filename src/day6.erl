-module(day6).
-export([part1/0, part2/0]).

input() ->
    [binary_to_integer(B) || B <- binary:split(hd(aoc_input:read(?MODULE, "^ *(.+) *$", [binary])), <<",">>, [global, trim_all])].

part1() ->
    after_days(80).

part2() ->
    after_days(256).

make_fish_tuple(FishList) ->
    Zeros = list_to_tuple([0 || _I <- lists:seq(0, 8)]),
    lists:foldl(fun (Fish, Acc) ->
        setelement(Fish + 1, Acc, element(Fish + 1, Acc) + 1)
    end, Zeros, FishList).

after_days(Days) ->
    FishTuple = make_fish_tuple(input()),
    lists:sum(tuple_to_list(lists:foldl(fun (_Day, Acc) ->
        next_day(Acc)
    end, FishTuple, lists:seq(1, Days)))).

next_day(FishTuple) ->
    Shifted = erlang:delete_element(1, FishTuple),
    WithEights = erlang:append_element(Shifted, element(1, FishTuple)),
    AndSixes = setelement(7, WithEights, element(7, WithEights) + element(1, FishTuple)),
    AndSixes.

next_day1(FishList) ->
    New = length(lists:filter(fun (X) -> X == 0 end, FishList)),
    Existing = lists:map(fun
        (0) -> 6;
        (X) -> X - 1
    end, FishList),
    Existing ++ lists:duplicate(New, 8).