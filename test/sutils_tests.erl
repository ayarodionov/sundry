%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2022 Anatoly Rodionov.  All Rights Reserved.
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
%%% @copyright 2022 Anatoly Rodionov
%%%
%%% @doc tests for edoph module
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------------------------------------------------------------------------------------

-module(sutils_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------
lcs_test() ->
    ?assertEqual("a",       sutils:lcs("ga s", "agc")),
    ?assertEqual("ac",      sutils:lcs("gac", "agcat")),
    ?assertEqual("lgrithm", sutils:lcs("logarithm", "algorithm")),
    ?assertEqual("lorithm", sutils:lcs("algorithm", "logarithm")),
    ?assertEqual("",        sutils:lcs("1234567890", "logarithm")),
    ok.

%-----------------------------------------------------------------------------------------------
lcs_len_test() ->
    ?assertEqual(1,  sutils:lcs_len("ga s", "agc")),
    ?assertEqual(2,  sutils:lcs_len("gac", "agcat")),
    ?assertEqual(7,  sutils:lcs_len("logarithm", "algorithm")),
    ?assertEqual(7,  sutils:lcs_len("algorithm", "logarithm")),
    ?assertEqual(0,  sutils:lcs_len("1234567890", "logarithm")),
    ok.

%-----------------------------------------------------------------------------------------------
 