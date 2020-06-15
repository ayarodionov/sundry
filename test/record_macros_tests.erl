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

?GET_RECORD_ALL_FIELDS(rrr).
?GET_RECORD_FIELD_BY_NAME(rrr).
?SET_RECORD_FIELD_BY_NAME(rrr).

%-----------------------------------------------------------------------------------------------

rrr_test() ->
    F = ?FIND_RECORD_INDEX_BY_NAME(rrr),
    ?assertMatch(3, F(record_info(fields, rrr), b, 2, F)),
    ?assertMatch([{a,1},{b,2},{c,3},{d,4}], rrr(#rrr{})),
	?assertMatch(1,   rrr(#rrr{}, a)),
    ?assertMatch(2,   rrr(#rrr{}, b)),
    ?assertMatch(rrr, rrr(#rrr{}, x)),
    ?assertMatch(22,  rrr(rrr(#rrr{}, b, 22), b)),
    ok.

%-------------------------------------------------------------------------------------------------
