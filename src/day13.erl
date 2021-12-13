-module(day13).
-export([part1/0, part2/0]).

input() ->
    Points = aoc_input:read(?MODULE, "^ *([\\d]+) *, *([\\d]+) *$", [integer, integer]),
    {Xs, Ys} = lists:unzip(Points),
    XMax = lists:max(Xs),
    YMax = lists:max(Ys),
    PointsSet = sets:from_list(Points),
    Paper = lists:map(fun (Y) ->
        lists:map(fun (X) ->
            sets:is_element({X, Y}, PointsSet)
        end, lists:seq(0, XMax))
    end, lists:seq(0, YMax)),
    Folds = aoc_input:read(?MODULE, "^ *fold along +([xy])=([\\d]+) *$", [atom, integer]),
    {Paper, Folds}.

part1() ->
    {Paper, Folds} = input(),
    case hd(Folds) of
        {x, X} -> count(fold_x(Paper, X));
        {y, Y} -> count(fold_y(Paper, Y))
    end.

part2() ->
    {Paper, Folds} = input(),
    display(lists:foldl(fun
        ({x, X}, Acc) -> fold_x(Acc, X);
        ({y, Y}, Acc) -> fold_y(Acc, Y)
    end, Paper, Folds)).

display(Paper) ->
    lists:map(fun (Line) ->
        lists:map(fun
            (false) -> $ ;
            (true) -> $#
        end, Line)
    end, Paper).

fold_x(Paper, X) ->
    Width = length(hd(Paper)),
    X = round(Width / 2 - 0.5),
    lists:map(fun (Line) ->
        LeftHalf = lists:sublist(Line, X),
        RightHalf = lists:sublist(lists:reverse(Line), X),
        lists:map(fun ({E1, E2}) -> E1 or E2 end, lists:zip(LeftHalf, RightHalf))
    end, Paper).

fold_y(Paper, Y) ->
    Height = length(Paper),
    UpperHalf = lists:sublist(Paper, Y),
    LowerHalf = lists:sublist(lists:reverse(Paper), Height - Y - 1),
    {Half1, Half2} = extend(UpperHalf, LowerHalf),
    lists:map(fun
        ({undefined, Line2}) ->
            Line2;
        ({Line1, undefined}) ->
            Line1;
        ({Line1, Line2}) ->
            lists:map(fun ({E1, E2}) ->
                E1 or E2
            end, lists:zip(Line1, Line2))
    end, lists:zip(Half1, Half2)).

extend(Half1, Half2) ->
    L1 = length(Half1),
    L2 = length(Half2),
    if
        L1 < L2 ->
            {lists:duplicate(L2 - L1, undefined) ++ Half1, Half2};
        L1 == L2 ->
            {Half1, Half2};
        L1 > L2 ->
            {Half1, lists:duplicate(L1 - L2, undefined) ++ Half2}
    end.

count(Paper) ->
    lists:foldl(fun(Line, LineAcc) ->
        lists:foldl(fun
            (true, Acc) -> Acc + 1;
            (false, Acc) -> Acc
        end, LineAcc, Line)
    end, 0, Paper).