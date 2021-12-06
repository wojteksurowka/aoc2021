-module(day2).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "([^ ]+) +([^ ]+)$", [atom, integer]).

part1() ->
    Instructions = input(),
    {Position, Depth} = lists:foldl(fun
        ({forward, Forward}, {PosAcc, DepthAcc}) -> {PosAcc + Forward, DepthAcc};
        ({up, Arg}, {PosAcc, DepthAcc}) -> {PosAcc, DepthAcc - Arg};
        ({down, Arg}, {PosAcc, DepthAcc}) -> {PosAcc, DepthAcc + Arg}
    end, {0, 0}, Instructions),
    Position * Depth.

part2() ->
    Instructions = input(),
    {Position, _Aim, Depth} = lists:foldl(fun
        ({forward, Forward}, {PosAcc, AimAcc, DepthAcc}) -> {PosAcc + Forward, AimAcc, DepthAcc + AimAcc * Forward};
        ({up, Arg}, {PosAcc, AimAcc, DepthAcc}) -> {PosAcc, AimAcc - Arg, DepthAcc};
        ({down, Arg}, {PosAcc, AimAcc, DepthAcc}) -> {PosAcc, AimAcc + Arg, DepthAcc}
    end, {0, 0, 0}, Instructions),
    Position * Depth.