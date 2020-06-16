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
%%% @doc This is a simple example how
%%% collection of macros for working with record field names
%%  can be used in gen_server.
%%%
%%% I use #s_t record as loop data in gen_server. Suppose that
%%% only record field a is of often use. For this particular field
%%% there are special cases in handle_cast and handle_call callbacks.
%%% All other filed can be manipulated with the help of functions
%%% s_t/2 and s_t/3. You do not need to write special accessors for them.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------------------------

-module(record_macros).
-behavior(gen_server).
-vsn(1.0).

%-----------------------------------------------------------------------------------------------
-include("record_macros.hrl").
%-----------------------------------------------------------------------------------------------
-export([start/0, start_link/0, init/1]).
-export([handle_cast/2, handle_call/3]).
-export([info/0, get/1, set/2]).


-define(MIN_PORT_NUMBER, 53).
-define(MAX_PORT_NUMBER, 65434).

%-----------------------------------------------------------------------------------------------
-record(s_t, {
	a   =  1 :: integer(),
	b   =  2 :: integer(),
	c   =  3 :: integer(),
	d   =  4 :: integer()
    }).
-type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------

-spec start() -> {ok, pid()}.
% @doc Starts {@module}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, {}, []).

-spec start_link() -> {ok, pid()}.
% @doc Starts {@module}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

-spec init(any()) -> {ok, s_t()}. 
% @doc Inits {@module}.
init({})  ->
    {ok, #s_t{}}.

%-----------------------------------------------------------------------------------------------
-spec set(atom(), term()) -> ok.
% @doc Sets any field in loop record
set(Field, Value) ->
    gen_server:cast(?MODULE, {set, Field, Value}).

-spec get(atom()) -> term().
% @doc Gets any field from loop record
get(Field) ->
    gen_server:call(?MODULE, {get, Field}).

-spec info() -> map().
% @doc Constructs map from loop record
info() ->
    gen_server:call(?MODULE, info).

%-----------------------------------------------------------------------------------------------
% Callbacks
%-----------------------------------------------------------------------------------------------
handle_cast({set, a, Value}, ST) ->
    {noreply, ST#s_t{a = Value}};

handle_cast({set, Field, Value}, ST) ->
    {noreply, s_t(ST, Field, Value)}.

handle_call({get, a}, _From, ST) ->
    {reply, ST#s_t.a, ST};

handle_call({get, Field}, _From, ST) ->
    {reply, s_t(ST, Field), ST};

handle_call(info, _From, ST) ->
    {reply, s_t(ST), ST}.

%-----------------------------------------------------------------------------------------------
% private
%-----------------------------------------------------------------------------------------------

?RECORD_TF_MAP(s_t).
?GET_RECORD_FIELD_BY_NAME(s_t).
?SET_RECORD_FIELD_BY_NAME(s_t).

%-----------------------------------------------------------------------------------------------
