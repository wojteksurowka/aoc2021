-module(day11).
-export([part1/0, part2/0]).

input() ->
    list_to_tuple(lists:map(fun (Line) ->
        list_to_tuple([X - $0 || X <- Line])
    end, aoc_input:read(?MODULE, "^ *([0-9]+) *$", [list]))).

part1() ->
    element(2, lists:foldl(fun (_I, {OctsAcc, TotalAcc}) ->
        {UpdatedOcts, Flashes} = step(OctsAcc),
        {UpdatedOcts, TotalAcc + Flashes}
    end, {input(), 0}, lists:seq(1, 100))).

part2() ->
    Octs = input(),
    AllCount = tuple_size(element(1, Octs)) * tuple_size(Octs),
    Runner = fun Run(Step, OctsAcc) ->
        case step(OctsAcc) of
            {_, AllCount} -> Step;
            {UpdatedOcts, _} -> Run(Step + 1, UpdatedOcts)
        end
    end,
    Runner(1, Octs).

step(Octs) ->
    Increased = increase(Octs),
    {FlashedOcts, Flashed} = flash(Increased, sets:new()),
    {zero_flashed(FlashedOcts, Flashed), sets:size(Flashed)}.

increase(Octs) ->
    Width = tuple_size(element(1, Octs)),
    Height = tuple_size(Octs),
    lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, YAcc) ->
            setel({X, Y}, el({X, Y}, YAcc) + 1, YAcc)
        end, XAcc, lists:seq(1, Height))
    end, Octs, lists:seq(1, Width)).

flash(Octs, Flashed) ->
    Width = tuple_size(element(1, Octs)),
    Height = tuple_size(Octs),
    {OctsAfterFlash, FlashedAfterFlash} = lists:foldl(fun (X, XAcc) ->
        lists:foldl(fun (Y, {OctsAcc, FlashedAcc}) ->
            case el({X, Y}, OctsAcc) > 9 andalso not sets:is_element({X, Y}, FlashedAcc) of
                true ->
                    UpdatedOcts = lists:foldl(fun (Adj, Acc) ->
                        setel(Adj, el(Adj, Acc) + 1, Acc)
                    end, OctsAcc, adjacents({X, Y}, OctsAcc)),
                    {UpdatedOcts, sets:add_element({X, Y}, FlashedAcc)};
                false ->
                    {OctsAcc, FlashedAcc}
            end
        end, XAcc, lists:seq(1, Height))
    end, {Octs, Flashed}, lists:seq(1, Width)),
    case sets:size(Flashed) < sets:size(FlashedAfterFlash) of
        true -> flash(OctsAfterFlash, FlashedAfterFlash);
        false -> {OctsAfterFlash, FlashedAfterFlash}
    end.

zero_flashed(Octs, Flashed) ->
    sets:fold(fun (XY, Acc) ->
        setel(XY, 0, Acc)
    end, Octs, Flashed).

adjacents({X, Y}, Octs) ->
    All = [{X - 1, Y - 1}, {X, Y - 1}, {X + 1, Y - 1}, {X - 1, Y}, {X + 1, Y}, {X - 1, Y + 1}, {X, Y + 1}, {X + 1, Y + 1}],
    Width = tuple_size(element(1, Octs)),
    Height = tuple_size(Octs),
    lists:filter(fun ({AX, AY}) ->
        AX > 0 andalso AX =< Width andalso AY > 0 andalso AY =< Height
    end, All).

el({X, Y}, Octs) ->
    element(X, element(Y, Octs)).

setel({X, Y}, Value, Octs) ->
    setelement(Y, Octs, setelement(X, element(Y, Octs), Value)).
