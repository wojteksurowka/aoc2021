-module(day23).
-export([part1/0, part2/0]).

input() ->
    [{At1, At2, At3, At4}, {Ab1, Ab2, Ab3, Ab4}] = aoc_input:read(?MODULE, "#([A-D])#([A-D])#([A-D])#([A-D])#", lists:duplicate(4, atom)),
    Occupied = maps:from_list([{{3, 2}, At1}, {{5, 2}, At2}, {{7, 2}, At3}, {{9, 2}, At4}, {{3, 3}, Ab1}, {{5, 3}, Ab2}, {{7, 3}, Ab3}, {{9, 3}, Ab4}]),
    lists:foldl(fun (XY, Acc) ->
        Acc#{XY => undefined}
    end, Occupied, hallway()).

part1() ->
    Burrow = input(),
    element(1, find_cost(Burrow, #{})).

part2() ->
    Burrow = input(),
    WithMovedDown = lists:foldl(fun (X, Acc) ->
        Acc#{{X, 5} => maps:get({X, 3}, Acc)}
    end, Burrow, [3, 5, 7, 9]),
    WithNewRows = lists:foldl(fun ({A, X, Y}, Acc) ->
        Acc#{{X, Y} => A}
    end, WithMovedDown, [{'D', 3, 3}, {'D', 3, 4}, {'C', 5, 3}, {'B', 5, 4}, {'B', 7, 3}, {'A', 7, 4}, {'A', 9, 3}, {'C', 9, 4}]),
    element(1, find_cost(WithNewRows, #{})).

find_cost(Burrow, Cache) ->
    case maps:get(Burrow, Cache, undefined) of
        undefined ->
            {Cost, UpdatedCache} = case complete(Burrow) of
                false ->
                    {Costs, CacheAfter} = lists:mapfoldl(fun ({NextBurrow, AddedCost}, Acc) ->
                        {NextCost, NextCache} = find_cost(NextBurrow, Acc),
                        case NextCost of
                            undefined -> {undefined, NextCache};
                            _ -> {NextCost + AddedCost, NextCache}
                        end
                    end, Cache, next(Burrow)),
                    OnlyCosts = lists:filter(fun is_number/1, Costs),
                    case OnlyCosts of
                        [] -> {undefined, CacheAfter};
                        _  -> {lists:min(OnlyCosts), CacheAfter}
                    end;
                true ->
                    {0, Cache}
            end,
            {Cost, UpdatedCache#{Burrow => Cost}};
        Result ->
            {Result, Cache}
    end.

next(Burrow) ->
    Moves = lists:flatten(lists:map(fun ({A, XY}) ->
        lists:map(fun (DXY) ->
            {A, XY, DXY}
        end, destinations(A, XY, Burrow))
    end, amphipods(Burrow))),
    FinishingMove = lists:search(fun ({_A, {_X, _Y}, {_DX, DY}}) -> DY > 1 end, Moves),
    MovesToUse = case FinishingMove of
        {value, Move} -> [Move];
        false -> Moves
    end,
    lists:map(fun ({A, XY, DXY}) ->
        {maps:put(DXY, A, maps:put(XY, undefined, Burrow)), cost(A, XY, DXY)}
    end, MovesToUse).

complete(Burrow) ->
    Depth = depth(Burrow),
    lists:all(fun (A) ->
        X = side_room(A),
        lists:all(fun (Y) -> maps:get({X, Y}, Burrow) =:= A end, lists:seq(2, Depth))
    end, ['A', 'B', 'C', 'D']).

cost(A, {X1, Y1}, {X2, Y2}) ->
    (Y1 - 1 + abs(X1 - X2) + Y2 - 1) * step_cost(A).

destinations(A, {X, 1}, Burrow) ->
    SideRoomX = side_room(A),
    case maps:get({SideRoomX, 2}, Burrow) of
        undefined ->
            {From, To} = case X < SideRoomX of
                true -> {X + 1, SideRoomX};
                false -> {SideRoomX, X - 1}
            end,
            FreeInHallway = lists:all(fun (XH) -> maps:get({XH, 1}, Burrow) =:= undefined end, lists:seq(From, To)),
            case FreeInHallway of
                true -> move_down(A, Burrow);
                false -> []
            end;
        _ ->
            []
    end;
destinations(A, {X, Y}, Burrow) ->
    SideRoomX = side_room(A),
    Depth = depth(Burrow),
    CanLeave = case {SideRoomX == X, Y == 2, Y == Depth} of
        {false, true, false} ->
            true;
        {false, false, _} ->
            lists:all(fun (SY) -> maps:get({X, SY}, Burrow) =:= undefined end, lists:seq(2, Y - 1));
        {true, false, true} ->
            false;
        {true, true, false} ->
            lists:any(fun (SY) -> maps:get({X, SY}, Burrow) =/= A end, lists:seq(3, Depth));
        {true, false, false} ->
            lists:any(fun (SY) -> maps:get({X, SY}, Burrow) =/= A end, lists:seq(Y + 1, Depth)) andalso
            lists:all(fun (SY) -> maps:get({X, SY}, Burrow) =:= undefined end, lists:seq(2, Y - 1))
    end,
    case CanLeave of
        true -> leaving(A, X, Burrow);
        false -> []
    end.

move_down(A, Burrow) ->
    SideRoomX = side_room(A),
    Depth = depth(Burrow),
    SideRoom = [maps:get({SideRoomX, SY}, Burrow) || SY <- lists:seq(2, Depth)],
    Occupied = lists:dropwhile(fun (SA) -> SA =:= undefined end, SideRoom),
    case Occupied of
        [] ->
            [{SideRoomX, Depth}];
        _ ->
            AllA = lists:all(fun (SA) -> SA =:= A end, Occupied),
            case AllA of
                true -> [{SideRoomX, Depth - length(Occupied)}];
                false -> []
            end
    end.

leaving_and_finishing(A, X, Burrow) ->
    SideRoomX = side_room(A),
    CanMoveHorizontally = case X < SideRoomX of
        true -> hallway_free(X, 1, SideRoomX, Burrow) =:= SideRoomX;
        false -> hallway_free(X, -1, SideRoomX, Burrow) =:= SideRoomX
    end,
    case CanMoveHorizontally of
        true -> move_down(A, Burrow);
        false -> []
    end.

leaving(A, X, Burrow) ->
    case maps:get({X, 1}, Burrow) of
        undefined ->
            case leaving_and_finishing(A, X, Burrow) of
                [] ->
                    Before = hallway_free(X, -1, 1, Burrow),
                    After = hallway_free(X, 1, 11, Burrow),
                    BeforeX1 = case Before < X of
                        true -> [{XH, 1} || XH <- lists:seq(Before, X - 1)];
                        false -> []
                    end,
                    AfterX1 = case After > X of
                        true -> [{XH, 1} || XH <- lists:seq(X + 1, After)];
                        false -> []
                    end,
                    lists:filter(fun ({XH, _Y}) -> not lists:member(XH, [3, 5, 7, 9]) end, BeforeX1 ++ AfterX1);
                LaFResult ->
                    LaFResult
            end;
        _ ->
            []
    end.

hallway_free(End, _Offset, End, _Burrow) ->
    End;
hallway_free(StartX, Offset, End, Burrow) ->
    NewX = StartX + Offset,
    case maps:get({NewX, 1}, Burrow) of
        undefined -> hallway_free(NewX, Offset, End, Burrow);
        _ -> StartX
    end.

hallway() ->
    [{X, 1} || X <- lists:seq(1, 11)].

side_room('A') -> 3;
side_room('B') -> 5;
side_room('C') -> 7;
side_room('D') -> 9.

step_cost('A') -> 1;
step_cost('B') -> 10;
step_cost('C') -> 100;
step_cost('D') -> 1000.

amphipods(Burrow) ->
    maps:fold(fun
        (_XY, undefined, Acc) -> Acc;
        (XY, A, Acc) -> [{A, XY} | Acc]
    end, [], Burrow).

depth(Burrow) ->
    (maps:size(Burrow) - 11) div 4 + 1.