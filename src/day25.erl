-module(day25).
-export([part1/0]).

input() ->
    list_to_tuple(lists:map(fun (Row) ->
        list_to_tuple([list_to_atom([C]) || C <- Row])
    end, aoc_input:read(?MODULE, "^ *([^ ]+) *$", [list]))).

part1() ->
    move(input(), 1).

move(Region, Count) ->
    {RegionAfterEast, EastMoved} = move_single('>', Region),
    {RegionAfterSouth, SouthMoved} = move_single(v, RegionAfterEast),
    case EastMoved orelse SouthMoved of
        true -> move(RegionAfterSouth, Count + 1);
        false -> Count
    end.

move_single(Tag, Region) ->
    Width = tuple_size(element(1, Region)),
    Height = tuple_size(Region),
    lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, {RegionAcc, AnythingMovedAcc}) ->
            try_move(Tag, X, Y, Region, RegionAcc, AnythingMovedAcc)
        end, XAcc, lists:seq(1, Height))
    end, {Region, false}, lists:seq(1, Width)).

try_move(Tag, X, Y, Original, Region, AnythingMovedBefore) ->
    case element(X, element(Y, Original)) of
        Tag ->
            {NX, NY} = next(Tag, X, Y, Original),
            case element(NX, element(NY, Original)) of
                '.' ->
                    {setel(X, Y, '.', setel(NX, NY, Tag, Region)), true};
                _ ->
                    {Region, AnythingMovedBefore}
            end;
        _ ->
            {Region, AnythingMovedBefore}
    end.

next('>', X, Y, Region) ->
    Width = tuple_size(element(1, Region)),
    case X of
        Width -> {1, Y};
        _ -> {X + 1, Y}
    end;
next(v, X, Y, Region) ->
    Height = tuple_size(Region),
    case Y of
        Height -> {X, 1};
        _ -> {X, Y + 1}
    end.

setel(X, Y, V, Region) ->
    setelement(Y, Region, setelement(X, element(Y, Region), V)).