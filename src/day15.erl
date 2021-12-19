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
    {Unvisited, BimapInfinity} = lists:foldl(fun (X, Acc) ->
        lists:foldl(fun (Y, {UnVisitedAcc, BimapAcc}) ->
            {sets:add_element({X, Y}, UnVisitedAcc), distance_bimap_infinity({X, Y}, BimapAcc)}
        end, Acc, lists:seq(1, Height))
    end, {sets:new(), distance_bimap_new()}, lists:seq(1, Width)),
    InitialBimap = distance_bimap_set({1, 1}, 0, BimapInfinity),
    distance_bimap_get({Width, Height}, dijkstra({1, 1}, Unvisited, InitialBimap, Map)).

dijkstra(Current, Unvisited, Bimap, Map) ->
    Width = tuple_size(element(1, Map)),
    Height = tuple_size(Map),
    UnvisitedNeighbours = lists:filter(fun (N) -> sets:is_element(N, Unvisited) end, neighbours(Current, Map)),
    CurrentDistance = distance_bimap_get(Current, Bimap),
    UpdatedBimap = lists:foldl(fun (N, Acc) ->
        NeighbourDistance = CurrentDistance + el(N, Map),
        case NeighbourDistance < distance_bimap_get(N, Acc) of
            true -> distance_bimap_set(N, NeighbourDistance, Acc);
            false -> Acc
        end
    end, Bimap, UnvisitedNeighbours),
    UpdatedUnvisited = sets:del_element(Current, Unvisited),
    case sets:is_element({Width, Height}, UpdatedUnvisited) of
        true ->
            BimapVisited = distance_bimap_set_visited(Current, UpdatedBimap),
            dijkstra(distance_bimap_min_unvisited(BimapVisited), UpdatedUnvisited, BimapVisited, Map);
        false ->
            UpdatedBimap
    end.

neighbours({X, Y}, Map) ->
    Width = tuple_size(element(1, Map)),
    Height = tuple_size(Map),
    All = [{X - 1, Y}, {X, Y - 1}, {X + 1, Y}, {X, Y + 1}],
    lists:filter(fun ({XF, YF}) ->
        XF >= 1 andalso XF =< Width andalso YF >= 1 andalso YF =< Height
    end, All).

distance_bimap_new() ->
    {#{}, #{}, #{}}.

distance_bimap_infinity(XY, {XY2D, D2UXYs, D2VXYs}) ->
    {XY2D#{XY => infinity}, D2UXYs#{infinity => sets:add_element(XY, maps:get(infinity, D2UXYs, sets:new()))}, D2VXYs}.

distance_bimap_set(XY, Distance, {XY2D, D2UXYs, D2VXYs}) ->
    PrevDistance = maps:get(XY, XY2D),
    {WasUnvisited, D2UXYsWithoutXY} = remove_from_d2xy(XY, PrevDistance, D2UXYs),
    {WasVisited, D2VXYsWithoutXY} = remove_from_d2xy(XY, PrevDistance, D2VXYs),
    case {WasUnvisited, WasVisited} of
        {true, false} -> {XY2D#{XY => Distance}, add_to_d2xy(XY, Distance, D2UXYsWithoutXY), D2VXYsWithoutXY};
        {false, true} -> {XY2D#{XY => Distance}, D2UXYsWithoutXY, add_to_d2xy(XY, Distance, D2VXYsWithoutXY)}
    end.

distance_bimap_get(XY, {XY2D, _D2UXYs, _D2VXYs}) ->
    maps:get(XY, XY2D).

distance_bimap_set_visited(XY, {XY2D, D2UXYs, D2VXYs}) ->
    Distance = maps:get(XY, XY2D),
    {XY2D, element(2, remove_from_d2xy(XY, Distance, D2UXYs)), add_to_d2xy(XY, Distance, D2VXYs)}.

distance_bimap_min_unvisited({_XY2D, D2UXYs, _D2VXYs}) ->
    Distance = hd(lists:sort(maps:keys(D2UXYs))),
    hd(sets:to_list(maps:get(Distance, D2UXYs))).

remove_from_d2xy(XY, Distance, D2XY) ->
    case maps:get(Distance, D2XY, undefined) of
        undefined ->
            {false, D2XY};
        PrevXYs ->
            WithoutXY = sets:del_element(XY, PrevXYs),
            UpdatedD2XY = case sets:size(WithoutXY) of
                0 -> maps:remove(Distance, D2XY);
                _ -> D2XY#{Distance := WithoutXY}
            end,
            {sets:size(WithoutXY) < sets:size(PrevXYs), UpdatedD2XY}
    end.

add_to_d2xy(XY, Distance, D2XY) ->
    case maps:get(Distance, D2XY, undefined) of
        undefined -> D2XY#{Distance => sets:from_list([XY])};
        XYs -> D2XY#{Distance => sets:add_element(XY, XYs)}
    end.

shift(X, Offset) -> ((X - 1) + Offset) rem 9 + 1.

el({X, Y}, Map) ->
    element(X, element(Y, Map)).