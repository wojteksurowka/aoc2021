-module(day24).
-export([part1/0, part2/0]).

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
    maps:get(0, steps(fun erlang:max/2, split(input()), 14)).

part2() ->
    maps:get(0, steps(fun erlang:min/2, split(input()), 14)).

steps(Comparator, Codes, 1) ->
    lists:foldl(fun (I, Acc) ->
        Result = calculate(0, I, Codes, 1),
        case maps:get(Result, Acc, undefined) of
            undefined -> Acc#{Result => I - $0};
            PR -> Acc#{Result => Comparator(I - $0, PR)}
        end
    end, #{}, lists:seq($1, $9));
steps(Comparator, Codes, N) ->
    maps:fold(fun (Init, Input, IAcc) ->
        lists:foldl(fun (I, Acc) ->
            Next = calculate(Init, I, Codes, N),
            case maps:get(Next, Acc, undefined) of
                undefined -> Acc#{Next => (I - $0) + Input * 10};
                PI -> Acc#{Next => Comparator((I - $0) + Input * 10, PI)}
            end
        end, IAcc, lists:seq($1, $9))
    end, #{}, steps(Comparator, Codes, N - 1)).

calculate(Init, SingleInput, Codes, Step) ->
    maps:get(z, execute({#{w => 0, x => 0, y => 0, z => Init}, [SingleInput]}, element(Step, Codes))).

split(Code) ->
    list_to_tuple(lists:map(fun lists:reverse/1, lists:reverse(lists:foldl(fun
        ({inp, R}, Acc) -> [[{inp, R}] | Acc];
        (Instruction, [H | T]) -> [[Instruction | H] | T]
    end, [], Code)))).

execute({Regs, Inputs}, Code) ->
    {OutputRegs, []} = lists:foldl(fun
        ({inp, R}, {RAcc, [H | T]}) -> {RAcc#{R := H - $0}, T};
        ({add, R, V}, {RAcc, IAcc}) -> {call(fun (X, Y) -> X + Y end, RAcc, R, V), IAcc};
        ({mul, R, V}, {RAcc, IAcc}) -> {call(fun (X, Y) -> X * Y end, RAcc, R, V), IAcc};
        ({'div', R, V}, {RAcc, IAcc}) -> {call(fun (X, Y) -> X div Y end, RAcc, R, V), IAcc};
        ({mod, R, V}, {RAcc, IAcc}) -> {call(fun (X, Y) -> X rem Y end, RAcc, R, V), IAcc};
        ({eql, R, V}, {RAcc, IAcc}) -> {call(fun (X, X) -> 1; (_X, _Y) -> 0 end, RAcc, R, V), IAcc}
    end, {Regs, Inputs}, Code),
    OutputRegs.

call(Fun, Regs, R, V) when is_atom(V) ->
    call(Fun, Regs, R, maps:get(V, Regs));
call(Fun, Regs, R, V) when is_integer(V) ->
    Regs#{R := Fun(maps:get(R, Regs), V)}.