%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2021 Anatoly Rodionov.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this fileß
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
%%% @doc Simple server for testing purposes. Used for {@link gsrv} testing.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------------------------

-module(test_srv).

-behavior(gen_server).

-vsn(1.0).

%-----------------------------------------------------------------------------------------------
-export([init/1, start/1, start_link/1]).

-export([handle_call/3]).

-export([get/1]).

-export([mstart/1, time_test/1, time_test/2]).

%-----------------------------------------------------------------------------------------------
-record(s_t, {sleep_time = 0  :: non_neg_integer()}).

-type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------

-spec start(non_neg_integer()) -> {ok, pid()}.

% @doc Starts {@module}.
start(SleepTime) ->
    Name = list_to_atom(integer_to_list(SleepTime)),
    gen_server:start({local, Name}, ?MODULE, SleepTime, []).

-spec start_link(non_neg_integer()) -> {ok, pid()}.

% @doc Starts {@module}.
start_link(SleepTime) ->
    Name = list_to_atom(integer_to_list(SleepTime)),
    gen_server:start_link({local, Name}, ?MODULE, SleepTime,
			  []).

-spec init(non_neg_integer()) -> {ok, s_t()}.

% @doc Inits {@module}.
init(SleepTime) -> {ok, #s_t{sleep_time = SleepTime}}.

%-----------------------------------------------------------------------------------------------

-spec get(atom() |
	  non_neg_integer()) -> non_neg_integer().
get(Name) when is_integer(Name) ->
    (?MODULE):get(list_to_atom(integer_to_list(Name)));
get(Name) when is_atom(Name) ->
    gen_server:call(Name, sleep_time).

%-----------------------------------------------------------------------------------------------
% Callbacks
%-----------------------------------------------------------------------------------------------

handle_call(sleep_time, _From, ST) ->
    timer:sleep(ST#s_t.sleep_time),
    {reply, ST#s_t.sleep_time, ST}.

%-----------------------------------------------------------------------------------------------

% L = [111,222,333,444,555,666,777,888,999].
% P = lists:map(fun(N) -> {ok, P} = test_srv:start(N), {P, sleep_time} end, L).
% [test_srv:get(N) || N <- L] .
% timer:tc(fun(Lst) -> lists:foreach(fun(N) -> test_srv:get(N) end, Lst) end, [L]).
% lists:sum(L).
% lists:max(L).

% {ok, P314} =test_srv:start(314).
% gen_server:call('314', sleep_time).
% gen_server:call(P314, sleep_time).
% R = gen_server:send_request(P314, sleep_time).


-spec mstart([non_neg_integer() | {non_neg_integer(), non_neg_integer() | infinity}]) ->
  [{pid(), sleep_time} | {pid(), sleep_time, non_neg_integer() | infinity}].
mstart([]) -> [];
mstart([{N, Timeout} | Tail]) -> [{save_start(N), sleep_time, Timeout} | mstart(Tail)];
mstart([N | Tail]) -> [{save_start(N), sleep_time} | mstart(Tail)].


-spec save_start(non_neg_integer()) -> pid().
save_start(N) when is_integer(N) andalso N >= 0 ->
  case test_srv:start(N) of 
    {ok, P} -> P;
    {error, {already_started, P}} -> P
  end.

-spec time_test
  ([{pid(), sleep_time} | {pid(), sleep_time, non_neg_integer() | infinity}]) ->
    [{atom(), non_neg_integer()}].
time_test(PLst) -> time_test(PLst, [multicall1, multicall2, multicall3, multicall4]).

-spec time_test
  ([{pid(), sleep_time} | {pid(), sleep_time, non_neg_integer() | infinity}], atom() | [atom()]) ->
    [{atom(), non_neg_integer()}].
time_test(PLst, Call) when is_atom(Call)->
  {T, _} = timer:tc(gsrv, Call, [PLst]),
  {Call, T/100};
time_test(PLst, Calls) -> [time_test(PLst, Call) || Call <- Calls].

%-----------------------------------------------------------------------------------------------
% 30> test_srv:time_test(test_srv:mstart([23,34,45,56,67]), multicall4).
% {multicall4,674.64}
% 32> test_srv:time_test(test_srv:mstart([2300,3400,4500,5600,6700])).            
% [{multicall1,202044.53},
%  {multicall2,84010.1},
%  {multicall3,50008.07},
%  {multicall4,50009.45}]
% 33> test_srv:time_test(test_srv:mstart([10000])).                   
% [{multicall1,50009.19},
%  {multicall2,50009.73},
%  {multicall3,50009.36},
%  {multicall4,50010.89}]
% 34> test_srv:time_test(test_srv:mstart([10000, 10000, 10000])).
% [{multicall1,150028.99},
%  {multicall2,150026.77},
%  {multicall3,50012.69},
%  {multicall4,50008.29}]
% 35>  test_srv:time_test(test_srv:mstart([230,340,450,560,670,760,870])).            
% [{multicall1,38869.64},
%  {multicall2,8708.96},
%  {multicall3,8710.1},
%  {multicall4,8710.14}]
%-----------------------------------------------------------------------------------------------
