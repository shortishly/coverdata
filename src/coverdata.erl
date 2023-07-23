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
         "input" => begin
                        {ok, CWD} = file:get_cwd(),
                        CWD
                    end,
         "precision" => "5",
         "level" => "module" }).

main([[$-, $- | Key], Value | Args], A) ->
    ?FUNCTION_NAME(Args, A#{Key => Value});

main([], Arg) ->
    process(
      maps:map(
        fun
            ("precision", Precision) ->
                list_to_integer(Precision);

            ("level", Level) ->
                list_to_atom(Level);

            (_, Value) ->
                Value
        end,
        Arg)).


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


report(#{"level" := module = Level, "precision" := Precision}) ->
    {result, Analysed, _} = cover:analyse(Level),
    {Report, Total} = lists:foldl(
                        fun
                            ({_, {0, 0}}, A) ->
                                A;

                            ({Module, {Cov, NotCov} = Counts},
                             {A, {TotalCov, TotalNotCov}}) ->
                                {A#{Module => percentage(Counts, Precision)},
                                 {Cov + TotalCov, NotCov + TotalNotCov}}
                        end,
                        {#{}, {0, 0}},
                        Analysed),
    Report#{total => percentage(Total, Precision)}.


-spec percentage({non_neg_integer(), non_neg_integer()}, non_neg_integer()) -> float().

percentage({Cov, NotCov}, Precision) ->
    precision(Cov * 100 / (Cov + NotCov), Precision).


-spec precision(float(), non_neg_integer()) -> float().

precision(Value, Digits) ->
    Decimals = float_to_binary(
                 Value,
                 [{decimals, decimals(Value, Digits)}]),

    case binary:split(Decimals, <<".">>) of
        [_, _] ->
            binary_to_float(Decimals);

        [_] ->
            binary_to_float(<<Decimals/bytes, ".0">>)
    end.


-spec decimals(float(), non_neg_integer()) -> integer().

decimals(Value, Digits) ->
    Digits - trunc(math:ceil(math:log10(trunc(abs(Value)) + 1))).
