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
    Start = {1, 1},
    End = {Width, Height},
    InitialBimap = distance_bimap_set(Start, 0, distance_bimap_new(Width, Height)),
    distance_bimap_get(End, dijkstra(Start, End, InitialBimap, Map)).

dijkstra(Current, End, Bimap, Map) ->
    UnvisitedNeighbours = lists:filter(fun (N) ->
        not distance_bimap_is_visited(N, Bimap)
    end, neighbours(Current, Map)),
    CurrentDistance = distance_bimap_get(Current, Bimap),
    BimapWithNeighbours = lists:foldl(fun (N, Acc) ->
        NeighbourDistance = CurrentDistance + el(N, Map),
        case NeighbourDistance < distance_bimap_get(N, Acc) of
            true -> distance_bimap_set(N, NeighbourDistance, Acc);
            false -> Acc
        end
    end, Bimap, UnvisitedNeighbours),
    UpdatedBimap = distance_bimap_set_visited(Current, BimapWithNeighbours),
    case distance_bimap_is_visited(End, UpdatedBimap) of
        true -> UpdatedBimap;
        false -> dijkstra(distance_bimap_min_unvisited(UpdatedBimap), End, UpdatedBimap, Map)
    end.

neighbours({X, Y}, Map) ->
    Width = tuple_size(element(1, Map)),
    Height = tuple_size(Map),
    lists:filter(fun ({XF, YF}) ->
        XF >= 1 andalso XF =< Width andalso YF >= 1 andalso YF =< Height
    end, [{X - 1, Y}, {X, Y - 1}, {X + 1, Y}, {X, Y + 1}]).

distance_bimap_new(Width, Height) ->
    lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, {XY2DAcc, D2UXYAcc, #{}}) ->
            {
                XY2DAcc#{{X, Y} => infinity},
                D2UXYAcc#{infinity => sets:add_element({X, Y}, maps:get(infinity, D2UXYAcc, sets:new()))},
                #{}
            }
        end, XAcc, lists:seq(1, Height))
    end, {#{}, #{}, #{}}, lists:seq(1, Width)).

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

distance_bimap_is_visited(XY, {XY2D, _D2UXYs, D2VXYs}) ->
    Distance = maps:get(XY, XY2D),
    case maps:get(Distance, D2VXYs, undefined) of
        undefined -> false;
        VisitedWithDistance -> sets:is_element(XY, VisitedWithDistance)
    end.

distance_bimap_min_unvisited({_XY2D, D2UXYs, _D2VXYs}) ->
    Distance = hd(lists:sort(maps:keys(D2UXYs))),
    random_set_element(maps:get(Distance, D2UXYs)).

random_set_element(Set) ->
    sets:fold(fun
        (Element, undefined) -> Element;
        (_Element, Found) -> Found
    end, undefined, Set).

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