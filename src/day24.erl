-module(day24).
-export([part1/0]).

input() ->
    lists:map(fun({Instr, Args}) ->
        Converted = lists:map(fun
            ([C]) when C >= $w, C =< $z -> list_to_atom([C]);
            (X) -> list_to_integer(X)
        end, string:split(Args, " ", all)),
        case Converted of
            [A] -> {Instr, A};
            [A, B] -> {Instr, A, B}
        end
    end, aoc_input:read(?MODULE, "^ *([a-z]+) +([a-z]+(?: +.+)?) *$", [atom, list])).

part1() ->
    Code = input(),
    %find(9#88888888888888, optimise(Code)).
    Optimised = optimise(Code),
    lists:map(fun (I) ->
        execute({0, "" ++ [I + $0]}, lists:sublist(Optimised, 1))
    end, lists:seq(1, 9)).
    %io:format("~p~n~p ~p~n", [Optimised, length(Code), length(Optimised)]),
    %execute(create(nona_to_decimal_string(9#00000000000000)), Optimised).

nona_to_decimal_string(N) ->
    [D + 1 || D <- io_lib:format("~14.9.0B", [N])].

find(N, Code) ->
    case N rem 100000 of
        0 -> io:format("~f~n", [(9#88888888888888 - N)/9#88888888888888 * 100]);
        _ -> ok
    end,
    case execute(create(nona_to_decimal_string(N)), Code) of
        0 -> list_to_integer(nona_to_decimal_string(N));
        _ -> find(N - 1, Code)
    end.

optimise(Code) ->
    Result = lists:reverse(optimise(Code, [])),
    case length(Result) < length(Code) of
        true -> optimise(Result);
        false -> Result
    end.

optimise([], Acc) ->
    Acc;
optimise([{mul, x, 0}, {add, x, z} | T], Acc) ->
    optimise([{x1} | T], Acc);
optimise([{x1}, {mod, x, 26} | T], Acc) ->
    optimise([{x2} | T], Acc);
optimise([{add, x, V}, {eql, x, w}, {eql, x, 0} | T], Acc) when is_integer(V) ->
    optimise([{x3, V} | T], Acc);
optimise([{mul, y, 0}, {add, y, V1}, {mul, y, x}, {add, y, V2} | T], Acc) when is_integer(V1), is_integer(V2) ->
    optimise([{y1, V1, V2} | T], Acc);
optimise([{mul, y, 0}, {add, y, w}, {add, y, V}, {mul, y, x} | T], Acc) when is_integer(V) ->
    optimise([{y2, V} | T], Acc);
optimise([{x2}, {'div', z, V1}, {x3, V2} | T], Acc) when is_integer(V1) ->
    optimise([{xz1, V1, V2} | T], Acc);
optimise([{xz1, V1, V2}, {y1, 25, 1}, {mul, z, y}, {y2, V3}, {add, z, y} | T], Acc) ->
    optimise([{xyz1, V1, V2, V3} | T], Acc);
optimise([{inp, w}, {xyz1, 1, V1, V2} | T], Acc) ->
    optimise([{ixyz1, V1, V2} | T], Acc);
optimise([{inp, w}, {xyz1, 26, V1, V2} | T], Acc) ->
    optimise([{ixyz2, V1, V2} | T], Acc);
optimise([H | T], Acc) ->
    optimise(T, [H | Acc]).

create(Input) ->
    {0, Input}.

execute({Regs, Inputs}, Code) ->
    element(1, lists:foldl(fun
        ({ixyz1, V1, V2}, {RAcc, [H | T]}) -> {ixyz1(H - $0, V1, V2, RAcc), T};
        ({ixyz2, V1, V2}, {RAcc, [H | T]}) -> {ixyz2(H - $0, V1, V2, RAcc), T}
    end, {Regs, Inputs}, Code)).

ixyz1(I, V1, V2, Acc) ->
    case Acc rem 26 + V1 == I of
        false ->
            Acc * 26 + (I + V2);
        true ->
            Acc * 25
    end.

ixyz2(I, V1, V2, Acc) ->
    case Acc rem 26 + V1 == I of
        false ->
            (Acc div 26) * 26 + (I + V2);
        true ->
            (Acc div 26) * 25
    end.