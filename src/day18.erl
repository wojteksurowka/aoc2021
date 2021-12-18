-module(day18).
-export([part1/0, part2/0]).

input() ->
    [element(2, erl_parse:parse_term(element(2, erl_scan:string(T ++ ".")))) || T <- aoc_input:read(?MODULE, "^ *([^ ]+) *$", [list])].

part1() ->
    [H | T] = input(),
    magnitude(add_all(H, T)).

part2() ->
    Lists = input(),
    Indexed = lists:zip(Lists, lists:seq(1, length(Lists))),
    lists:max(lists:flatten(lists:foldl(fun ({List1, I}, OAcc) ->
        lists:foldl(fun ({List2, J}, IAcc) ->
            case I == J of
                true -> IAcc;
                false -> [magnitude(reduce([List1, List2])) | IAcc]
            end
        end, OAcc, Indexed)
    end, [], Indexed))).

add_all(ListL, []) ->
    ListL;
add_all(ListL, [ListR | T]) ->
    add_all(reduce([ListL, ListR]), T).

reduce(List) ->
    {Updated, UpdatedList} = case reduce_explode(List, 0) of
        {not_reduced, NotUpdatedList} -> reduce_split(NotUpdatedList);
        {_Actions, ExplodedList} -> {true, ExplodedList}
    end,
    case Updated of
        true -> reduce(UpdatedList);
        false -> UpdatedList
    end.

magnitude([L, R]) ->
    3 * magnitude(L) + 2 * magnitude(R);
magnitude(X) ->
    X.

reduce_explode([L, R], Nested) when is_number(L), is_number(R), Nested >= 4 ->
    {[{add_to_last, L}, {add_to_first, R}], 0};
reduce_explode(X, _Nested) when is_number(X) ->
    {not_reduced, X};
reduce_explode([], _Nested) ->
    {not_reduced, []};
reduce_explode([H | T], Nested) ->
    {HResult, UpdatedH} = reduce_explode(H, Nested + 1),
    case HResult of
        not_reduced ->
            {TResult, UpdatedT} = reduce_explode(T, Nested),
            case TResult of
                not_reduced ->
                    {not_reduced, [UpdatedH | UpdatedT]};
                Actions ->
                    {UpdatedActions, HAfterActions} = apply_actions(Actions, add_to_last, UpdatedH),
                    {UpdatedActions, [HAfterActions | UpdatedT]}
            end;
        Actions ->
            {UpdatedActions, UpdatedT} = apply_actions(Actions, add_to_first, T),
            {UpdatedActions, [UpdatedH | UpdatedT]}
    end.

reduce_split(X) when is_number(X), X >= 10 ->
    {true, [floor(X / 2), ceil(X / 2)]};
reduce_split(X) when is_number(X) ->
    {false, X};
reduce_split([L, R]) ->
    case reduce_split(L) of
        {true, UpdatedL} ->
            {true, [UpdatedL, R]};
        {false, L} ->
            case reduce_split(R) of
                {true, UpdatedR} -> {true, [L, UpdatedR]};
                {false, R} -> {false, [L, R]}
            end
    end.

apply_actions(Actions, Action, Subject) ->
    case proplists:get_value(Action, Actions) of
        undefined ->
            {Actions, Subject};
        Value ->
            case apply_action(Action, Subject, Value) of
                {true, Result} -> {proplists:delete(Action, Actions), Result};
                {false, Subject} -> {Actions, Subject}
            end
    end.

apply_action(_Action, [], _X) ->
    {false, []};
apply_action(_Action, Subject, X) when is_number(Subject) ->
    {true, X + Subject};
apply_action(add_to_first, [First | T], X) when is_number(First) ->
    {true, [X + First | T]};
apply_action(add_to_first, [First | T], X) when is_list(First) ->
    {Added, Result} = apply_action(add_to_first, First, X),
    {Added, [Result | T]};
apply_action(add_to_last, [Last], X) when is_number(Last) ->
    {true, [X + Last]};
apply_action(add_to_last, [Last], X) when is_list(Last) ->
    {Added, Result} = apply_action(add_to_last, Last, X),
    {Added, [Result]};
apply_action(add_to_last, [H | T], X) ->
    {Added, Result} = apply_action(add_to_last, T, X),
    {Added, [H | Result]}.