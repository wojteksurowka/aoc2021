-module(day4).
-export([part1/0, part2/0]).

input() ->
    Numbers = [binary_to_integer(B) || B <- binary:split(hd(aoc_input:read(?MODULE, "^(\\d+,.*)$", [binary])), <<",">>, [global, trim_all])],
    BoardLines = aoc_input:read(?MODULE, "^ *(\\d+) +(\\d+) +(\\d+) +(\\d+) +(\\d+)$", [integer, integer, integer, integer, integer]),
    Boards = lists:foldr(fun ({Line, Index}, Acc) ->
        case Index rem 5 of
            0 -> [[tuple_to_list(Line)] | Acc];
            _ -> [[tuple_to_list(Line) | hd(Acc)] | tl(Acc)]
        end
    end, [], lists:zip(BoardLines, lists:seq(1, length(BoardLines)))),
    {Numbers, Boards}.

part1() ->
    {Numbers, Boards} = input(),
    lists:foldl(fun
        (Number, {BoardsAcc, undefined}) ->
            UpdatedBoards = [mark(Number, Board) || Board <- BoardsAcc],
            case lists:search(fun winning/1, UpdatedBoards) of
                {value, Board} ->
                    Unmarked = lists:filter(fun erlang:is_number/1, lists:flatten(Board)),
                    {[], lists:sum(Unmarked) * Number};
                false ->
                    {UpdatedBoards, undefined}
            end;
        (_, {BoardsAcc, Result}) ->
            {BoardsAcc, Result}
    end, {Boards, undefined}, Numbers).

part2() ->
    {Numbers, Boards} = input(),
    lists:foldl(fun
        (_Number, Result) when is_number(Result) ->
            Result;
        (Number, [Last]) ->
            Marked = mark(Number, Last),
            case winning(Marked) of
                true ->
                    Unmarked = lists:filter(fun erlang:is_number/1, lists:flatten(Marked)),
                    lists:sum(Unmarked) * Number;
                false ->
                    [Marked]
            end;
        (Number, Acc) ->
            UpdatedBoards = [mark(Number, Board) || Board <- Acc],
            lists:filter(fun (Board) -> not winning(Board) end, UpdatedBoards)
    end, Boards, Numbers).

mark(Number, Board) ->
    lists:map(fun (Line) ->
        lists:map(fun
            (X) when X == Number -> undefined;
            (X) -> X
        end, Line)
    end, Board).

winning(Board) ->
    case winning_line(Board) of
        true -> true;
        false -> winning_line(transpose(Board))
    end.

winning_line(Board) ->
    lists:any(fun (Line) -> lists:all(fun erlang:is_atom/1, Line) end, Board).

transpose(Board) ->
    lists:map(fun (Index) ->
        lists:map(fun (Line) ->
            lists:nth(Index, Line)
        end, Board)
    end, lists:seq(1, 5)).