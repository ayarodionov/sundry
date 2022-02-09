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
%%% @doc tests for lisub module
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------------------------------------------------------------------------------------

-module(lisub_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------
max_second_test() ->
    ?assertEqual(0,  lisub:max_second(1,[])),
    ?assertEqual(0,  lisub:max_second(1, [{1,1}])),
    ?assertEqual(1, lisub:max_second(2, [{12,12}, {1,1}, {3,3}])),
    ?assertEqual(3, lisub:max_second(4, [{12,12}, {1,1}, {3,3}])),
    ok.

%-----------------------------------------------------------------------------------------------
ln_test() ->
    ?assertEqual(0,  lisub:ln([])),
    ?assertEqual(1,  lisub:ln([1])),
    ?assertEqual(1,  lisub:ln([1,1,1,1,1])),
    ?assertEqual(1,  lisub:ln([2,1,1,1,1])),
    ?assertEqual(1,  lisub:ln([2,2,1,1,1])),
    ?assertEqual(2,  lisub:ln([1,2,1,1,1])),
    ?assertEqual(2,  lisub:ln([1,2,2,1,1])),
    ?assertEqual(6,  lisub:ln([0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15])),
    ok.

%-----------------------------------------------------------------------------------------------
max_insert_test() ->
    ?assertEqual([{1,1}],  lisub:max_insert(1,[])),
    ?assertEqual([{1,1}],  lisub:max_insert(1, [{1,1}])),
    ?assertEqual([{2,1}, {3,3}, {5,2}], lisub:max_insert(2, [{3,3}, {5,2}])),
    ?assertEqual([{1,1}, {2,2}, {3,3}], lisub:max_insert(2, [{1,1}, {3,3}])),
    ?assertEqual([{1,1}, {3,3}, {4,4}], lisub:max_insert(4, [{1,1}, {3,3}])),
    ok.

%-----------------------------------------------------------------------------------------------
li_test() ->
    ?assertEqual(0,  lisub:li([])),
    ?assertEqual(1,  lisub:li([1])),
    ?assertEqual(1,  lisub:li([1,1,1,1,1])),
    ?assertEqual(1,  lisub:li([2,1,1,1,1])),
    ?assertEqual(1,  lisub:li([2,2,1,1,1])),
    ?assertEqual(2,  lisub:li([1,2,1,1,1])),
    ?assertEqual(2,  lisub:li([1,2,2,1,1])),
    ?assertEqual(6,  lisub:ln([0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15])),
    ok.

%-----------------------------------------------------------------------------------------------
