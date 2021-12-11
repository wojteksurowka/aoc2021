-module(day8).
-export([part1/0, part2/0]).

input() ->
    Lines = aoc_input:read(?MODULE, "^ *(.+) *\\| *(.+) *$", [binary, binary]),
    [{binary:split(Left, <<" ">>, [global, trim_all]), binary:split(Right, <<" ">>, [global, trim_all])} || {Left, Right} <- Lines].

part1() ->
    {_Lefts, Rights} = lists:unzip(input()),
    lists:sum([count1478(Right) || Right <- Rights]).

part2() ->
    lists:sum(lists:map(fun ({Left, Right}) ->
        Map = analyse_line(Left ++ Right),
        list_to_integer(lists:flatten(lists:map(fun (Entry) ->
            digit(Entry, Map)
        end, Right)))
    end, input())).

count1478(Right) ->
    length(lists:filter(fun (X) -> lists:member(byte_size(X), [2, 4, 3, 7]) end, Right)).

analyse_line(Line) ->
    FullMap = lists:foldl(fun (From, Acc) ->
        Acc#{From => lists:seq($a, $g)}
    end, #{}, lists:seq($a, $g)),
    singlesise(lists:foldl(fun (Entry, Acc) ->
        EntryList = ordsets:from_list(binary_to_list(Entry)),
        case byte_size(Entry) of
            2 -> remove_unique(EntryList, "cf", Acc);
            4 -> remove_unique(EntryList, "bcdf", Acc);
            3 -> remove_unique(EntryList, "acf", Acc);
            6 -> remove_remaining(EntryList, "cde", Acc);
            _ -> Acc
        end
    end, FullMap, Line)).

remove_unique(FromList, ToList, Map) ->
    maps:map(fun (Key, Value) ->
        case lists:member(Key, FromList) of
            true -> ordsets:intersection(Value, ToList);
            false -> ordsets:subtract(Value, ToList)
        end
    end, Map).

remove_remaining(EntryList, OnlyThese, Map) ->
    maps:map(fun (Key, Value) ->
        case lists:member(Key, EntryList) of
            true -> Value;
            false -> ordsets:intersection(Value, OnlyThese)
        end
    end, Map).

singlesise(Map) ->
    Singles = ordsets:from_list(maps:fold(fun (_Key, Value, Acc) ->
        case Value of
            [Single] -> [Single | Acc];
            _ -> Acc
        end
    end, [], Map)),
    maps:map(fun (_Key, Value) ->
        case Value of
            [_] -> Value;
            _ -> ordsets:subtract(Value, Singles)
        end
    end, Map).

digit(Entry, Map) ->
    digit(ordsets:from_list(lists:map(fun (C) -> hd(maps:get(C, Map)) end, binary_to_list(Entry)))).

digit("abcefg") -> "0";
digit("cf") -> "1";
digit("acdeg") -> "2";
digit("acdfg") -> "3";
digit("bcdf") -> "4";
digit("abdfg") -> "5";
digit("abdefg") -> "6";
digit("acf") -> "7";
digit("abcdefg") -> "8";
digit("abcdfg") -> "9".