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


-export([main/1]).
-export([report/1]).


main(Args) ->
    ?FUNCTION_NAME(
       Args,
       #{"format" => "json",
         "input" => cwd(),
         "level" => "module" }).

main([[$-, $- | Key], Value | Args], A) ->
    ?FUNCTION_NAME(Args, A#{Key => Value});

main([], Arg) ->
    process(
      maps:map(
        fun
            ("level", Level) ->
                list_to_atom(Level);

            (_, Value) ->
                Value
        end,
        Arg)).


cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD.


process(#{"input" := Input, "output" := Output} = Arg) ->
    case filelib:is_dir(Input) of
        true ->
            ok = import_dir(Input);

        false ->
            ok = cover:import(Input)
    end,
    ok = file:write_file(Output, format_report(Arg)).


import_dir(Directory) ->
    true = lists:all(
             fun
                 (Result) ->
                     Result == ok
             end,
             [cover:import(CoverData)
              || CoverData
                     <- filelib:wildcard(
                          filename:join(
                            Directory,
                            "**/*.coverdata"))]),
    ok.


-spec format_report(#{}) -> iodata().

format_report(#{"format" := "json"} = Arg) ->
    jsx:encode(report(Arg)).


report(#{"level" := module = Level}) ->
    {result, Analysed, _} = cover:analyse(Level),
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


-spec percentage({non_neg_integer(), non_neg_integer()}) -> non_neg_integer().

percentage({Cov, NotCov}) ->
    round(Cov * 100 / (Cov + NotCov)).
