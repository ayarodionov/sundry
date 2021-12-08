%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2021 Anatoly Rodionov.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
%%% @copyright 2021 Anatoly Rodionov
%%%
%%% @doc Tests for emedian
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------------------------

-module(emedian_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------
simple_test() ->
    ?assertEqual(5,     emedian:median([9, 1, 0, 2, 3, 4, 6, 8, 7, 10, 5])),
    ?assertEqual(5,     emedian:median(lists:seq(1,9))),
    ?assertEqual(5.5,   emedian:median(lists:seq(1,10))),
    ?assertEqual(133,   emedian:median(lists:duplicate(17,133))),
    ?assertEqual(133.0, emedian:median(lists:duplicate(18,133))),
    ?assertEqual(4.45,  emedian:median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])),
    ?assertEqual(4.4,   emedian:median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3])),
    ?assertEqual(44,    emedian:median([41, 56, 72, 17, 93, 44, 32])),
    ?assertEqual(42.5,  emedian:median([41, 72, 17, 93, 44, 32])),

    ?assertEqual(5,     emedian:sort_median([9, 1, 0, 2, 3, 4, 6, 8, 7, 10, 5])),
    ?assertEqual(5,     emedian:sort_median(lists:seq(1,9))),
    ?assertEqual(5.5,   emedian:sort_median(lists:seq(1,10))),
    ok.

%-------------------------------------------------------------------------------------------------

