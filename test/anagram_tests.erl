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
%%% @doc tests for anagram module
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------------------------------------------------------------------------------------

-module(anagram_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------
as_sum_test() ->
    ?assertEqual(57888, anagram:as_sum("cab")),
    ?assertEqual(57888, anagram:as_sum("abc")),
    ?assertEqual(57888, anagram:as_sum("bac")),
    ok.

%-----------------------------------------------------------------------------------------------
eq_test() ->
    ?assert(anagram:eq("abc", "abc")),
    ?assert(anagram:eq("abc", "acb")),
    ?assert(anagram:eq("abc", "cba")),

    ?assertNot(anagram:eq("abc", "xba")),
    ?assertNot(anagram:eq("abc", "cbae")),
    ok.

%-----------------------------------------------------------------------------------------------
prime_test() ->
    ?assertEqual(11, anagram:prime(5)),
    ?assertEqual(281, anagram:prime(60)),
    ?assertEqual(523, anagram:prime(99)),
    ok.

%-----------------------------------------------------------------------------------------------
as_prime_prod_test() ->
    ?assertEqual(317, anagram:as_prime_prod("a")),
    ?assertEqual(331, anagram:as_prime_prod("b")),
    ?assertEqual(35360399, anagram:as_prime_prod("abc")),
    ?assertEqual(35360399, anagram:as_prime_prod("cab")),
    ok.

%-----------------------------------------------------------------------------------------------
eqp_test() ->
    ?assert(anagram:eqp("abc", "abc")),
    ?assert(anagram:eqp("abc", "acb")),
    ?assert(anagram:eqp("abc", "cba")),

    ?assertNot(anagram:eqp("abc", "xba")),
    ?assertNot(anagram:eqp("abc", "cbae")),
    ?assertNot(anagram:eqp("abc", "axcvbaebn")),
    ok.

%-------------------------------------------------------------------------------------------------
