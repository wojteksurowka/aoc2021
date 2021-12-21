-module(day21).
-export([part1/0, part2/0]).

input() ->
    list_to_tuple(aoc_input:read(?MODULE, "^ *Player \\d starting position: (\\d+) *$", [integer])).

part1() ->
    play1(1, 1, input(), {0, 0}, 1).

part2() ->
    element(1, split(1, input(), {0, 0}, {0, 0}, #{})).

play1(Player, Die, Positions, Scores, Count) ->
    {Roll, NextDie} = die3(Die),
    NewPosition = (element(Player, Positions) + Roll - 1) rem 10 + 1,
    NewScore = element(Player, Scores) + NewPosition,
    OtherPlayer = Player rem 2 + 1,
    case NewScore >= 1000 of
        true ->
            element(OtherPlayer, Scores) * Count * 3;
        false ->
            play1(OtherPlayer, NextDie, setelement(Player, Positions, NewPosition), setelement(Player, Scores, NewScore), Count + 1)
    end.

split(Player, Positions, Scores, Wins, Cache) ->
    Results = [X + Y + Z || X <- lists:seq(1, 3), Y <- lists:seq(1, 3), Z <- lists:seq(1, 3)],
    lists:foldl(fun (Result, {WinsAcc, CacheAcc}) ->
        play2(Player, Result, Positions, Scores, WinsAcc, CacheAcc)
    end, {Wins, Cache}, Results).

play2(Player, Roll, Positions, Scores, Wins, Cache) ->
    Key = {Player, Roll, Positions, Scores},
    case maps:get(Key, Cache, undefined) of
        undefined ->
            NewPosition = (element(Player, Positions) + Roll - 1) rem 10 + 1,
            NewScore = element(Player, Scores) + NewPosition,
            OtherPlayer = Player rem 2 + 1,
            {{Wins1, Wins2}, UpdatedCache} = case NewScore >= 21 of
                true ->
                    {setelement(Player, {0, 0}, 1), Cache};
                false ->
                    NewPositions = setelement(Player, Positions, NewPosition),
                    NewScores = setelement(Player, Scores, NewScore),
                    split(OtherPlayer, NewPositions, NewScores, {0, 0}, Cache)
            end,
            {add({Wins1, Wins2}, Wins), UpdatedCache#{Key => {Wins1, Wins2}}};
        {Wins1, Wins2} ->
            {add({Wins1, Wins2}, Wins), Cache}
    end.

add({L1, L2}, {R1, R2}) ->
    {L1 + R1, L2 + R2}.

die3(Die) ->
    {R1, Die1} = die(Die),
    {R2, Die2} = die(Die1),
    {R3, Die3} = die(Die2),
    {R1 + R2 + R3, Die3}.

die(100) ->
    {100, 1};
die(Die) ->
    {Die, Die + 1}.