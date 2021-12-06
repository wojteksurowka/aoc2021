-module(aoc_input).
-export([read/3]).

read(Module, RE, Types) ->
    [$d, $a, $y | Day] = atom_to_list(Module),
    {ok, Input} = file:read_file("input/day" ++ Day ++ ".in"),
    Lines = binary:split(Input, <<"\n">>, [global, trim_all]),
    {ok, CompiledRE} = re:compile(RE),
    lists:map(fun (Line) ->
        {match, [_All | Groups]} = re:run(Line, CompiledRE),
        Converted = lists:map(fun
            ({Group, atom}) -> binary_to_atom(binary:part(Line, Group));
            ({Group, integer}) -> binary_to_integer(binary:part(Line, Group))
        end, lists:zip(Groups, Types)),
        case Types of
            [_Single] -> hd(Converted);
            _ -> list_to_tuple(Converted)
        end
    end, Lines).
