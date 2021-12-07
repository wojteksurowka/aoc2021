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
    Numbers = input(),
    most_common(1, Numbers) * least_common(1, Numbers).

most_common(Index, Numbers) ->
    What = case zeroones(Index, Numbers) of
        {Zeros, Ones} when Zeros > Ones -> $0;
        _ -> $1
    end,
    Filtered = lists:filter(fun (Number) -> binary_part(Number, Index - 1, 1) =:= <<What>> end, Numbers),
    case Filtered of
        [Single] -> binary_to_integer(Single, 2);
        _ -> most_common(Index + 1, Filtered)
    end.

least_common(Index, Numbers) ->
    What = case zeroones(Index, Numbers) of
        {Zeros, Ones} when Zeros > Ones -> $1;
        _ -> $0
    end,
    Filtered = lists:filter(fun (Number) -> binary_part(Number, Index - 1, 1) =:= <<What>> end, Numbers),
    case Filtered of
        [Single] -> binary_to_integer(Single, 2);
        _ -> least_common(Index + 1, Filtered)
    end.

zeroones(Index, Numbers) ->
    lists:foldl(fun (Binary, {Zeros, Ones}) ->
        case binary_part(Binary, Index - 1, 1) of
            <<"0">> -> {Zeros + 1, Ones};
            <<"1">> -> {Zeros, Ones + 1}
        end
    end, {0, 0}, Numbers).