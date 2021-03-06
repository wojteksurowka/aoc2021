-module(day19).
-export([part1/0, part2/0]).

input() ->
    lists:foldl(fun
        ({[$-, $-, $- | _], _Scanner, _}, Acc) ->
            Acc#{maps:size(Acc) => sets:new()};
        ({X, Y, Z}, Acc) ->
            Scanner = maps:size(Acc) - 1,
            Acc#{Scanner => sets:add_element({list_to_integer(X), list_to_integer(Y), list_to_integer(Z)}, maps:get(Scanner, Acc))}
    end, #{}, aoc_input:read(?MODULE, "^ *(.+)[, ](-?\\d+)[, ](.+) *$", [list, list, list])).

part1() ->
    Scanners = input(),
    Scanner2Info = find_region(Scanners, sets:new()),
    length(map_beacons(Scanner2Info, Scanners)).

part2() ->
    Positions = maps:fold(fun (_Scanner, {FR, Delta}, Acc) ->
        [add(map({0, 0, 0}, FR), Delta) | Acc]
    end, [], find_region(input(), sets:new())),
    lists:max(lists:foldl(fun ({LX, LY, LZ}, LAcc) ->
        lists:foldl(fun ({RX, RY, RZ}, RAcc) ->
            [abs(LX - RX) + abs(LY - RY) + abs(LZ - RZ) | RAcc]
        end, LAcc, Positions)
    end, [], Positions)).

map_beacons(Scanner2Info, Scanners) ->
    lists:usort(maps:fold(fun (Scanner, {FR, Delta}, Acc) ->
        sets:fold(fun (Beacon, BAcc) ->
            [add(map(Beacon, FR), Delta) | BAcc]
        end, Acc, maps:get(Scanner, Scanners))
    end, [], Scanner2Info)).

find_region(Scanners, Misses) ->
    Scanner = hd(lists:sort(maps:keys(Scanners))),
    {Scanner2Info, [], _Misses} = find_next(#{Scanner => {[hd(positions())], {0, 0, 0}}}, lists:delete(Scanner, maps:keys(Scanners)), Scanners, Misses),
    Scanner2Info.

find_next(Done, [], _Scanners, Misses) ->
    {Done, [], Misses};
find_next(Done, NotDone, Scanners, Misses) ->
    case find_overlapping(Done, NotDone, Scanners, Misses) of
        {undefined, UpdatedMisses} ->
            {Done, NotDone, UpdatedMisses};
        {{Scanner, Info}, UpdatedMisses} ->
            find_next(Done#{Scanner => Info}, lists:delete(Scanner, NotDone), Scanners, UpdatedMisses)
    end.

find_overlapping(Done, NotDone, Scanners, Misses) ->
    maps:fold(fun
        (DoneScanner, {DoneFR, DoneDelta}, {undefined, MissesAcc}) ->
            lists:foldl(fun
                (NotDoneScanner, {undefined, NDMissesAcc}) ->
                    case overlapping(DoneScanner, NotDoneScanner, 12, Scanners, NDMissesAcc) of
                        {undefined, UpdatedMisses} ->
                            {undefined, UpdatedMisses};
                        {{FR, DXYZ}, UpdatedMisses} ->
                            NotDoneInfo = {[FR | DoneFR], add(DoneDelta, map(DXYZ, DoneFR))},
                            {{NotDoneScanner, NotDoneInfo}, UpdatedMisses}
                    end;
                (_NotDoneScanner, Result) ->
                    Result
            end, {undefined, MissesAcc}, NotDone);
        (_DoneScanner, _DoneScannerInfo, Result) ->
            Result
    end, {undefined, Misses}, Done).

overlapping(ScannerL, ScannerR, AtLeast, Scanners, Misses) ->
    Key = list_to_tuple(lists:sort([ScannerL, ScannerR])),
    case sets:is_element(Key, Misses) of
        true ->
            {undefined, Misses};
        false ->
            OverlappingResult = sets:fold(fun
                (LXYZ, undefined) ->
                    sets:fold(fun
                        (RXYZ, undefined) ->
                            lists:foldl(fun
                                (FR, undefined) ->
                                    DXYZ = subtract(LXYZ, map(RXYZ, FR)),
                                    Mapped = map_coords(maps:get(ScannerR, Scanners), FR, DXYZ),
                                    case sets:size(sets:intersection(maps:get(ScannerL, Scanners), Mapped)) >= AtLeast of
                                        true -> {FR, DXYZ};
                                        false -> undefined
                                    end;
                                (_FR, Result) ->
                                    Result
                            end, undefined, positions());
                        (_R, Result) ->
                            Result
                    end, undefined, maps:get(ScannerR, Scanners));
                (_L, Result) ->
                    Result
            end, undefined, maps:get(ScannerL, Scanners)),
            UpdatedMisses = case OverlappingResult of
                undefined -> sets:add_element(Key, Misses);
                _ -> Misses
            end,
            {OverlappingResult, UpdatedMisses}
    end.

positions() ->
    [{F, R} || F <- lists:seq(1, 6), R <- lists:seq(1, 4)].

add({X, Y, Z}, {DX, DY, DZ}) ->
    {X + DX, Y + DY, Z + DZ}.

subtract({X, Y, Z}, {DX, DY, DZ}) ->
    {X - DX, Y - DY, Z - DZ}.

map_coords(Coords, FR, Delta) ->
    sets:fold(fun (XYZ, Acc) ->
        sets:add_element(add(map(XYZ, FR), Delta), Acc)
    end, sets:new(), Coords).

map(XYZ, FRs) when is_list(FRs) ->
    lists:foldl(fun (FR, Acc) ->
        map(Acc, FR)
    end, XYZ, FRs);
map({X, Y, Z}, {F, R}) ->
    {FX, FY, FZ} = case F of
        1 -> {X, Y, Z};
        2 -> {Y, -X, Z};
        3 -> {Z, Y, -X};
        4 -> {-X, Y, -Z};
        5 -> {-Y, X, Z};
        6 -> {-Z, Y, X}
    end,
    case R of
        1 -> {FX, FY, FZ};
        2 -> {FX, FZ, -FY};
        3 -> {FX, -FY, -FZ};
        4 -> {FX, -FZ, FY}
    end.