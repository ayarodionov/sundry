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

-module(edioph_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------
gcd_test() ->
    ?assertEqual(2,  edioph:gcd(4, 2)),
    ?assertEqual(2,  edioph:gcd(2, 4)),
    ?assertEqual(1,  edioph:gcd(5, 2)),
    ?assertEqual(6,  edioph:gcd(48, 18)),
    ?assertEqual(6,  edioph:gcd(54, 24)),
    ok.

%-----------------------------------------------------------------------------------------------
lcm_test() ->
    ?assertEqual(12,  edioph:lcm(4, 6)),
    ?assertEqual(42,  edioph:lcm(21, 6)),
    ok.

%-----------------------------------------------------------------------------------------------
