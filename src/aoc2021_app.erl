%%%-------------------------------------------------------------------
%% @doc aoc2021 public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc2021_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc2021_sup:start_link().

stop(_State) ->
    ok.

start() ->

    Day = 4,
    Part = 2,

    Module = list_to_atom("day" ++ integer_to_list(Day)),
    Function = list_to_atom("part" ++ integer_to_list(Part)),
    Result = Module:Function(),
    io:format("~nDay ~p Part ~p: ~p~n~n", [Day, Part, Result]),
    halt().