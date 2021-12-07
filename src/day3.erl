-module(day3).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "([01]+)", [binary]).

part1() ->
    Numbers = input(),
    ZeroOnes = lists:map(fun (Index) -> zeroones(Index, Numbers) end, lists:seq(1, byte_size(hd(Numbers)))),
    Gamma = list_to_integer(lists:map(fun
        ({Zeros, Ones}) when Zeros >= Ones -> $0;
        ({Zeros, Ones}) when Zeros < Ones -> $1
    end, ZeroOnes), 2),
    Epsilon = list_to_integer(lists:map(fun
        ({Zeros, Ones}) when Zeros >= Ones -> $1;
        ({Zeros, Ones}) when Zeros < Ones -> $0
    end, ZeroOnes), 2),
    Gamma * Epsilon.

part2() ->
    ok.

zeroones(Index, Numbers) ->
    lists:foldl(fun (Binary, {Zeros, Ones}) ->
        case binary_part(Binary, Index - 1, 1) of
            <<"0">> -> {Zeros + 1, Ones};
            <<"1">> -> {Zeros, Ones + 1}
        end
    end, {0, 0}, Numbers).