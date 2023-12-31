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


-module(coverdata_tests).


-include_lib("eunit/include/eunit.hrl").


scan_test() ->
    cover:import("test/scran.coverdata.0"),
    ?assertEqual(
       #{total => 68,
         scran_combinator => 96,
         scran_debug => 0,
         scran_number_be => 6,
         scran_number => 24,
         scran_multi => 97,
         scran_bytes => 83,
         scran_branch => 100,
         scran_sequence => 96,
         scran_result => 33,
         scran_number_le => 0,
         scran_bits => 100,
         scran_character_complete => 99},
       coverdata:report(#{"level" => module})).
