%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(coverdata).


-export([import/0]).
-export([main/1]).
-export([report/1]).


main(Args) ->
    ?FUNCTION_NAME(Args, #{"format" => "json", "level" => "module" }).

main([[$-, $- | Key], Value | Args], A) ->
    ?FUNCTION_NAME(Args, A#{Key => Value});

main([],
     #{"input" := Input,
       "output" := Output,
       "level" := Level,
       "format" := Format}) ->
    ok = cover:import(Input),
    ok = file:write_file(
           Output,
           report(Format, Level)).


report("json", Level) ->
    jsx:encode(report(Level)).

report("module" = Level) ->
    {result, Analysed, _} = cover:analyse(list_to_atom(Level)),
    {Report, Total} = lists:foldl(
                        fun
                            ({_, {0, 0}}, A) ->
                                A;

                            ({Module, {Cov, NotCov} = Counts},
                             {A, {TotalCov, TotalNotCov}}) ->
                                {A#{Module => percentage(Counts)},
                                 {Cov + TotalCov, NotCov + TotalNotCov}}
                        end,
                        {#{}, {0, 0}},
                        Analysed),
    Report#{total => percentage(Total)}.


percentage({Cov, NotCov}) ->
    precision(Cov * 100 / (Cov + NotCov), 5).


precision(Value, Digits) ->
    Decimals = float_to_binary(
                 Value,
                 [{decimals, Digits - trunc(math:ceil(math:log10(trunc(abs(Value)) + 1)))}]),
    case binary:split(Decimals, <<".">>) of
        [_, _] ->
            binary_to_float(Decimals);

        [_] ->
            binary_to_float(<<Decimals/bytes, ".0">>)
    end.


import() ->
    {ok, CWD} = file:get_cwd(),
    [cover:import(CoverData)
     || CoverData
            <- lists:map(
                 fun
                     (Relative) ->
                         filename:join(CWD, Relative)
                 end,
                 filelib:wildcard("**/*.coverdata")) -- cover:imported()].
