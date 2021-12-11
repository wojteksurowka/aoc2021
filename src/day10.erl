-module(day10).
-export([part1/0, part2/0]).

input() ->
    aoc_input:read(?MODULE, "^ *([([{<>}\\])]+) *$", [list]).

part1() ->
    lists:sum([score(element(1, eat_chunk(Input, []))) || Input <- input()]).

part2() ->
    LegalRemaining = lists:filtermap(fun
        ({$ , Remaining}) -> {true, lists:map(fun reverse/1, Remaining)};
        (_) -> false
    end, [eat_chunk(Input, []) || Input <- input()]),
    Scores = lists:sort([score_remaining(Remaining) || Remaining <- LegalRemaining]),
    lists:nth(length(Scores) div 2 + 1, Scores).

eat_chunk([], Stack) -> {$ , Stack};
eat_chunk([$( | T], Stack) -> eat_chunk(T, [$( | Stack]);
eat_chunk([$[ | T], Stack) -> eat_chunk(T, [$[ | Stack]);
eat_chunk([${ | T], Stack) -> eat_chunk(T, [${ | Stack]);
eat_chunk([$< | T], Stack) -> eat_chunk(T, [$< | Stack]);
eat_chunk([$) | T], [$( | Stack]) -> eat_chunk(T, Stack);
eat_chunk([$] | T], [$[ | Stack]) -> eat_chunk(T, Stack);
eat_chunk([$} | T], [${ | Stack]) -> eat_chunk(T, Stack);
eat_chunk([$> | T], [$< | Stack]) -> eat_chunk(T, Stack);
eat_chunk([Illegal | _T], Stack) -> {Illegal, Stack}.

reverse($() -> $);
reverse($[) -> $];
reverse(${) -> $};
reverse($<) -> $>.

score($)) -> 3;
score($]) -> 57;
score($}) -> 1197;
score($>) -> 25137;
score($ ) -> 0.

other_score($)) -> 1;
other_score($]) -> 2;
other_score($}) -> 3;
other_score($>) -> 4.

score_remaining(Remaining) ->
    lists:foldl(fun (C, Acc) ->
        Acc * 5 + other_score(C)
    end, 0, Remaining).