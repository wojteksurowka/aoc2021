-module(day15).
-export([part1/0, part2/0]).

input() ->
    list_to_tuple([list_to_tuple([L - $0|| L <- Line]) || Line <- aoc_input:read(?MODULE, "^(\\d+)$", [list])]).

part1() ->
    do_map(input()).

part2() ->
    Map = input(),
    Width = tuple_size(element(1, Map)),
    Height = tuple_size(Map),
    BigMap = list_to_tuple(lists:map(fun (Y) ->
        list_to_tuple(lists:map(fun (X) ->
            shift(el({(X - 1) rem Width + 1, (Y - 1) rem Height + 1}, Map), (X - 1) div Width + (Y - 1) div Height)
        end, lists:seq(1, Width * 5)))
    end, lists:seq(1, Height * 5))),
    do_map(BigMap).

do_map(Map) ->
    Width = tuple_size(element(1, Map)),
    Height = tuple_size(Map),
    Horizontal = lists:foldl(fun (X, Acc) -> el({X, 1}, Map) + Acc end, 0, lists:seq(2, Width)),
    AndVertical = lists:foldl(fun (Y, Acc) -> el({Width, Y}, Map) + Acc end, Horizontal, lists:seq(1, Height)),
    element(1, minpathto({Width, Height}, Map, #{}, [{Width, Height}], AndVertical, 0)).

shift(X, Offset) -> ((X - 1) + Offset) rem 9 + 1.

minpathto({X, Y}, _Map, Cache, Visited, APath, Partial) when APath < Partial ->
    {false, Cache#{{X, Y, length(Visited)} => false}};
minpathto({1, 2}, Map, Cache, _Visited, _APath, _Partial) ->
    {el({1, 2}, Map), Cache};
minpathto({2, 1}, Map, Cache, _Visited, _APath, _Partial) ->
    {el({2, 1}, Map), Cache};
minpathto({X, Y}, Map, Cache, Visited, APath, Partial) ->
    VisitedLen = length(Visited),
    case maps:get({X, Y, VisitedLen}, Cache, undefined) of
        undefined ->
            {Paths, UpdatedCache} = lists:mapfoldl(fun (Pred, CacheAcc) ->
                minpathto(Pred, Map, CacheAcc, ordsets:add_element({X, Y}, Visited), APath, Partial + el({X, Y}, Map))
            end, Cache, preds({X, Y}, Map, Visited)),
            RealPaths = lists:filter(fun is_number/1, Paths),
            Result = case RealPaths of
                [] -> false;
                _ -> lists:min(RealPaths) + el({X, Y}, Map)
            end,
            {Result, UpdatedCache#{{X, Y, VisitedLen} => Result}};
        Result ->
            {Result, Cache}
    end.

preds({X, Y}, Map, Visited) ->
    Width = tuple_size(element(1, Map)),
    Height = tuple_size(Map),
    All = [{X - 1, Y}, {X, Y - 1}, {X + 1, Y}, {X, Y + 1}],
    Inside = lists:filter(fun ({XF, YF}) ->
        XF >= 1 andalso XF =< Width andalso YF >= 1 andalso YF =< Height
    end, All),
    ordsets:subtract(ordsets:from_list(Inside), Visited).

el({X, Y}, Map) ->
    element(X, element(Y, Map)).