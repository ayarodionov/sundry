%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2020 Anatoly Rodionov.  All Rights Reserved.
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
%%% @copyright 2020 Anatoly Rodionov
%%%
%%% @doc tests for record macros
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------------------------

-module(record_macros_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-include("record_macros.hrl").

%-----------------------------------------------------------------------------------------------

-record(rrr, {a = 1, b = 2, c = 3, d = 4}).

%-----------------------------------------------------------------------------------------------

?RECORD_TF_MAP(rrr).
?GET_RECORD_FIELD_BY_NAME(rrr).
?SET_RECORD_FIELD_BY_NAME(rrr).

%-----------------------------------------------------------------------------------------------

rrr_test() ->
    F = ?FIND_RECORD_INDEX_BY_NAME,
    ?assertMatch(3, F(record_info(fields, rrr), b, 2, F)),
    ?assertMatch([{a,1},{b,2},{c,3},{d,4}], maps:to_list(rrr(#rrr{}))),
    ?assertMatch(#rrr{}, rrr(#{a=>1, b=>2, c=>3, d=>4})),
	?assertMatch(1,   rrr(#rrr{}, a)),
    ?assertMatch(2,   rrr(#rrr{}, b)),
    ?assertMatch(rrr, rrr(#rrr{}, x)),
    ?assertMatch(22,  rrr(rrr(#rrr{}, b, 22), b)),
    ok.

%-------------------------------------------------------------------------------------------------

st_test() ->
    {ok, _} = record_macros:start(),
    ?assertMatch([{a,1},{b,2},{c,3},{d,4}], maps:to_list(record_macros:info())),
    ?assertMatch(1,   record_macros:get(a)),
    ?assertMatch(2,   record_macros:get(b)),
    ?assertMatch(ok,  record_macros:set(a, 11)),
    ?assertMatch(ok,  record_macros:set(b, 22)),
    ?assertMatch(11,  record_macros:get(a)),
    ?assertMatch(22,  record_macros:get(b)),
    ok.

%-------------------------------------------------------------------------------------------------
