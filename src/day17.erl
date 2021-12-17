-module(day17).
-export([part1/0, part2/0]).

input() ->
    hd(aoc_input:read(?MODULE, "^ *target area: x=(\\d+)\\.\\.(\\d+), y=(-\\d+)\\.\\.(-\\d+) *$", [integer, integer, integer, integer])).

part1() ->
    Target = {_FromX, ToX, FromY, _ToY} = input(),
    lists:max(lists:filtermap(fun ({VX, VY}) ->
        case path({VX, VY}, [{0, 0}], Target) of
            false -> false;
            Path -> {true, lists:max(element(2, lists:unzip(Path)))}
        end
    end, [{VX, VY} || VX <- lists:seq(1, ToX), VY <- lists:seq(FromY, -FromY)])).

part2() ->
    Target = {_FromX, ToX, FromY, _ToY} = input(),
    length(lists:filter(fun ({VX, VY}) ->
        path({VX, VY}, [{0, 0}], Target) =/= false
    end, [{VX, VY} || VX <- lists:seq(1, ToX), VY <- lists:seq(FromY, -FromY)])).

path({VX, VY}, [{X, Y} | _] = Acc, Target) ->
    {{NewVX, NewVY}, {NewX, NewY}} = step({VX, VY}, {X, Y}),
    case {in_target({NewX, NewY}, Target), no_hope({NewX, NewY}, Target)} of
        {true, false} -> lists:reverse([{NewX, NewY} | Acc]);
        {false, true} -> false;
        {false, false} -> path({NewVX, NewVY}, [{NewX, NewY} | Acc], Target)
    end.

step({VX, VY}, {X, Y}) ->
    NewVX = if
        VX > 0 -> VX - 1;
        VX < 0 -> VX + 1;
        VX == 0 -> 0
    end,
    {{NewVX, VY - 1}, {X + VX, Y + VY}}.

in_target({X, Y}, {FromX, ToX, FromY, ToY}) ->
    X >= FromX andalso X =< ToX andalso Y >= FromY andalso Y =< ToY.

no_hope({X, Y}, {_FromX, ToX, FromY, _ToY}) ->
    X > ToX orelse Y < FromY.