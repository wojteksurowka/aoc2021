-module(day14).
-export([part1/0, part2/0]).

input() ->
    Polymer = hd(aoc_input:read(?MODULE, "^ *([A-Z]+) *$", [list])),
    Rules = lists:foldl(fun ({Before, After, Insert}, Acc) ->
        Acc#{{Before, After} => Insert}
    end, #{}, aoc_input:read(?MODULE, "^ *([A-Z])([A-Z]) *-> *([A-Z]) *$", [character, character, character])),
    {Polymer, Rules}.

part1() ->
    do_steps(10, input()).

part2() ->
    do_steps(40, input()).

do_steps(Steps, {Polymer, Rules}) ->
    {Recursive, _} = lists:foldl(fun (Pair, {Acc, CacheAcc}) ->
        {Value, UpdatedCache} = steps(Steps, Pair, Rules, CacheAcc),
        {sum(Value, Acc), UpdatedCache}
    end, {#{}, #{}}, pairs(Polymer)),
    Quantities = maps:map(fun (_K, V) ->
        V div 2
    end, add_to_quantities(hd(Polymer), add_to_quantities(lists:last(Polymer), Recursive))),
    Sorted = lists:sort(maps:values(Quantities)),
    lists:last(Sorted) - hd(Sorted).

steps(Steps, Pair, Rules, Cache) ->
    case maps:get({Steps, Pair}, Cache, undefined) of
        undefined ->
            case {Steps, Pair} of
                {0, {C, C}} ->
                    R = #{C => 2},
                    {R, Cache#{{Steps, Pair} => R}};
                {0, {F, S}} ->
                    R = #{F => 1, S => 1},
                    {R, Cache#{{Steps, Pair} => R}};
                {_, {F, S}} ->
                    case maps:get({F, S}, Rules, undefined) of
                        undefined ->
                            steps(0, {F, S}, Rules, Cache);
                        I ->
                            {FR, CacheF} = steps(Steps - 1, {F, I}, Rules, Cache),
                            {SR, CacheS} = steps(Steps - 1, {I, S}, Rules, CacheF),
                            Result = sum(FR, SR),
                            {Result, CacheS#{{Steps, Pair} => Result}}
                    end
            end;
        Value ->
            {Value, Cache}
    end.

sum(Q1, Q2) ->
    maps:fold(fun (K, V, Acc) ->
        Acc#{K => maps:get(K, Acc, 0) + V}
    end, Q1, Q2).

add_to_quantities(C, Quantities) ->
    Quantities#{C => maps:get(C, Quantities, 0) + 1}.

pairs([F, S]) -> [{F, S}];
pairs([F, S | _] = List) -> [{F, S} | pairs(tl(List))].
