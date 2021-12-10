-module(day5).
-export([part1/0, part2/0]).

input() ->
    [{{X1, Y1}, {X2, Y2}} || {X1, Y1, X2, Y2} <- aoc_input:read(?MODULE, "^ *(\\d+),(\\d+) *-> *(\\d+),(\\d+) *$", [integer, integer, integer, integer])].

part1() ->
    Lines = input(),
    count_ge_2(mark_hv(Lines, create_board(maxxy(Lines)))).

part2() ->
    Lines = input(),
    count_ge_2(mark_all(Lines, create_board(maxxy(Lines)))).

count_ge_2(Board) ->
    lists:foldl(fun (Line, LineAcc) ->
        LineAcc + length(lists:filter(fun (V) -> V >= 2 end, tuple_to_list(Line)))
    end, 0, tuple_to_list(Board)).

maxxy(Lines) ->
    {Froms, Tos} = lists:unzip(Lines),
    {Xs, Ys} = lists:unzip(Froms ++ Tos),
    {lists:max(Xs), lists:max(Ys)}.

create_board({MaxX, MaxY}) ->
    list_to_tuple(lists:map(fun (_Y) ->
        list_to_tuple([0 || _X <- lists:seq(0, MaxX)])
    end, lists:seq(0, MaxY))).

mark_hv(Lines, Board) ->
    lists:foldl(fun
        ({{X, Y1}, {X, Y2}}, Acc) ->
            lists:foldl(fun (Y, IAcc) -> add(X, Y, IAcc) end, Acc, seq(Y1, Y2));
        ({{X1, Y}, {X2, Y}}, Acc) ->
            lists:foldl(fun (X, IAcc) -> add(X, Y, IAcc) end, Acc, seq(X1, X2));
        (_, Acc) ->
            Acc
    end, Board, Lines).

mark_all(Lines, Board) ->
    lists:foldl(fun (Line, Acc) ->
        lists:foldl(fun ({X, Y}, LAcc) -> add(X, Y, LAcc) end, Acc, points(Line))
    end, Board, Lines).

points({{X, Y1}, {X, Y2}}) -> [{X, Y} || Y <- seq(Y1, Y2)];
points({{X1, Y}, {X2, Y}}) -> [{X, Y} || X <- seq(X1, X2)];
points({{X1, Y1}, {X2, Y2}}) -> lists:zip(seq(X1, X2), seq(Y1, Y2)).

add(X, Y, Board) ->
    Horizontal = element(Y + 1, Board),
    UpdatedHorizontal = setelement(X + 1, Horizontal, element(X + 1, Horizontal) + 1),
    setelement(Y + 1, Board, UpdatedHorizontal).

seq(From, To) when From =< To -> lists:seq(From, To);
seq(From, To) when From > To -> lists:reverse(lists:seq(To, From)).