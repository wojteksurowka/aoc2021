-module(day12).
-export([part1/0, part2/0]).

input() ->
    lists:foldl(fun ({From, To}, Acc) ->
        OneWay = Acc#{From => [To | maps:get(From, Acc, [])]},
        OneWay#{To => [From | maps:get(To, Acc, [])]}
    end, #{}, aoc_input:read(?MODULE, "^ *([a-zA-Z]+)-([a-zA-Z]+) *$", [atom, atom])).

part1() ->
    length(next([start], input(), [],
        fun (Node, Path) -> is_small(Node) andalso lists:member(Node, Path) end)).

part2() ->
    length(next([start], input(), [], fun
        (start, _Path) -> true;
        (Node, Path) -> is_small(Node) andalso lists:member(Node, Path) andalso has_small_duplicate(Path)
    end)).

next([Last | _] = RPath, Graph, Paths, Checker) ->
    lists:foldl(fun
        ('end', Acc) ->
            [lists:reverse(['end' | RPath]) | Acc];
        (Next, Acc) ->
            case Checker(Next, RPath) of
                true ->
                    Acc;
                false ->
                    next([Next | RPath], Graph, Acc, Checker)
            end
    end, Paths, maps:get(Last, Graph, [])).

is_small(Atom) ->
    First = hd(atom_to_list(Atom)),
    First >= $a andalso First =< $z.

has_small_duplicate(Path) ->
    CountMap = lists:foldl(fun (Node, Acc) ->
        case is_small(Node) of
            true -> Acc#{Node => maps:get(Node, Acc, 0) + 1};
            false -> Acc
        end
    end, #{}, Path),
    lists:any(fun (C) -> C >= 2 end, maps:values(CountMap)).