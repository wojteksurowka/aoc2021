-module(day20).
-export([part1/0, part2/0]).

input() ->
    All = aoc_input:read(?MODULE, "^ *([^ ]+) *$", [list]),
    [InputLen, AlgoLen] = lists:usort([length(Line) || Line <- All]),
    [Algo] = lists:filter(fun (Line) -> length(Line) == AlgoLen end, All),
    Inputs = lists:filter(fun (Line) -> length(Line) == InputLen end, All),
    {hashdot_to_binary(Algo), list_to_tuple([hashdot_to_binary(Input) || Input <- Inputs])}.

part1() ->
    iterate(2).

part2() ->
    iterate(50).

iterate(Count) ->
    {Algo, Image} = input(),
    count_ones(lists:foldl(fun (_I, Acc) ->
        process(Algo, Acc)
    end, extend(Image, $0), lists:seq(1, Count))).

hashdot_to_binary(Line) ->
    list_to_tuple(lists:map(fun
        ($#) -> $1;
        ($.) -> $0
    end, Line)).

process(Algo, Image) ->
    Width = tuple_size(element(1, Image)),
    Height = tuple_size(Image),
    extend(lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, YAcc) ->
            Index = list_to_integer(
                [el(X - 1, Y - 1, Image), el(X, Y - 1, Image), el(X + 1, Y - 1, Image),
                 el(X - 1, Y, Image), el(X, Y, Image), el(X + 1, Y, Image),
                 el(X - 1, Y + 1, Image), el(X, Y + 1, Image), el(X + 1, Y + 1, Image)], 2),
            setel(X, Y, element(Index + 1, Algo), YAcc)
        end, XAcc, lists:seq(0, Height - 1))
    end, Image, lists:seq(0, Width - 1))).

count_ones(Image) ->
    Width = tuple_size(element(1, Image)),
    Height = tuple_size(Image),
    lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, YAcc) ->
            case el(X, Y, Image) of
                $0 -> YAcc;
                $1 -> YAcc + 1
            end
        end, XAcc, lists:seq(0, Height - 1))
    end, 0, lists:seq(0, Width - 1)).

extend(Image) ->
    extend(Image, element(1, element(1, Image))).

extend(Image, Element) ->
    Width = tuple_size(element(1, Image)),
    Horizontally = lists:map(fun (Line) ->
        list_to_tuple([Element, Element, Element | tuple_to_list(Line)] ++ [Element, Element, Element])
    end, tuple_to_list(Image)),
    EmptyLine = list_to_tuple(lists:duplicate(Width + 6, Element)),
    list_to_tuple([EmptyLine, EmptyLine, EmptyLine | Horizontally] ++ [EmptyLine, EmptyLine, EmptyLine]).

el(X, Y, Image) ->
    Width = tuple_size(element(1, Image)),
    Height = tuple_size(Image),
    case X < 0 orelse X >= Width orelse Y < 0 orelse Y >= Height of
        true -> element(1, element(1, Image));
        false -> element(X + 1, element(Y + 1, Image))
    end.

setel(X, Y, V, Image) ->
    setelement(Y + 1, Image, setelement(X + 1, element(Y + 1, Image), V)).