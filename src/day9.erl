-module(day9).
-export([part1/0, part2/0]).

input() ->
    list_to_tuple([list_to_tuple([L - $0|| L <- Line]) || Line <- aoc_input:read(?MODULE, "^(\\d+)$", [list])]).

part1() ->
    HeightMap = input(),
    Width = tuple_size(element(1, HeightMap)),
    Height = tuple_size(HeightMap),
    lists:foldl(fun (Y, YAcc) ->
        lists:foldl(fun (X, XAcc) ->
            case is_low(X, Y, HeightMap) of
                true -> XAcc + el({X, Y}, HeightMap) + 1;
                false -> XAcc
            end
        end, YAcc, lists:seq(1, Width))
    end, 0, lists:seq(1, Height)).

part2() ->
    HeightMap = input(),
    Width = tuple_size(element(1, HeightMap)),
    Height = tuple_size(HeightMap),
    Available = lists:foldl(fun (Y, YAcc) ->
        lists:foldl(fun (X, XAcc) ->
            case el({X, Y}, HeightMap) of
                9 -> XAcc;
                _ -> sets:add_element({X, Y}, XAcc)
            end
        end, YAcc, lists:seq(1, Width))
    end, sets:new(), lists:seq(1, Height)),
    [S1, S2, S3] = lists:sublist(lists:reverse(lists:sort([sets:size(Basin) || Basin <- basins(Available, [], HeightMap)])), 3),
    S1 * S2 * S3.

basins(Available, Basins, HeightMap) ->
    case sets:to_list(Available) of
        [] ->
            Basins;
        [XY | _] ->
            {NewBasin, UpdatedAvailable} = grow(sets:from_list([XY]), sets:del_element(XY, Available), HeightMap),
            basins(UpdatedAvailable, [NewBasin | Basins], HeightMap)
    end.

grow(Basin, Available, HeightMap) ->
    {ExtendedBasin, UpdatedAvailable} = sets:fold(fun (XY, Acc) ->
        lists:foldl(fun (AXY, {BasinAcc, AvailableAcc}) ->
            case not sets:is_element(AXY, BasinAcc) andalso sets:is_element(AXY, AvailableAcc) of
                true -> {sets:add_element(AXY, BasinAcc), sets:del_element(AXY, AvailableAcc)};
                false -> {BasinAcc, AvailableAcc}
            end
        end, Acc, adjacents(XY, HeightMap))
    end, {Basin, Available}, Basin),
    case sets:size(ExtendedBasin) > sets:size(Basin) of
        true -> grow(ExtendedBasin, UpdatedAvailable, HeightMap);
        false -> {ExtendedBasin, UpdatedAvailable}
    end.

is_low(X, Y, HeightMap) ->
    lists:all(fun (Adjacent) ->
        el({X, Y}, HeightMap) < el(Adjacent, HeightMap)
    end, adjacents({X, Y}, HeightMap)).

adjacents({X, Y}, HeightMap) ->
    Width = tuple_size(element(1, HeightMap)),
    Height = tuple_size(HeightMap),
    case X of
        1 -> [{X + 1, Y} | adjacents_y({X, Y}, Height)];
        Width -> [{X - 1, Y} | adjacents_y({X, Y}, Height)];
        _ -> [{X - 1, Y}, {X + 1, Y} | adjacents_y({X, Y}, Height)]
    end.

adjacents_y({X, Y}, Height) ->
    case Y of
        1 -> [{X, Y + 1}];
        Height -> [{X, Y - 1}];
        _ -> [{X, Y - 1}, {X, Y + 1}]
    end.

el({X, Y}, HeightMap) -> element(X, element(Y, HeightMap)).