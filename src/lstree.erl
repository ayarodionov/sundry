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
%%% @doc pruning algorithm in Erlang
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-------------------------------------------------------------------------------------------------

-module(lstree).

%-------------------------------------------------------------------------------------------------

-export([from_list/1, from_list/2, to_list/1]).
-export([min/1, max/1]).
-export([min_max/1, max_min/1]).
-export([min_max_p/1, max_min_p/1]).
-export([map/2, map/3, map/4]).

%-------------------------------------------------------------------------------------------------

-define(ALPHA_MAX, 16#7FFFFFFFFFFFFFF).   % Erlang max integer (60 bit)
-define(BETA_MIN,  -?ALPHA_MAX).

%-------------------------------------------------------------------------------------------------

-record(lsnode,{
	level    = 0      :: non_neg_integer(),
	score    = false  :: integer() | false,
	children = []     :: [lsnode()]   
	}).
-type lsnode()       :: #lsnode{}. 

%-------------------------------------------------------------------------------------------------
-spec from_list([number()] | number()) -> lsnode().

from_list(Arg) -> ?MODULE:from_list(Arg, 0).

%-------------------------------------------------------------------------------------------------
-spec from_list([number()] | number(), non_neg_integer()) -> lsnode().

from_list(S, L) when is_list(S) -> 
	#lsnode{children = [?MODULE:from_list(A, L + 1) || A <- S], level = L};
from_list(S, L) when is_number(S) -> #lsnode{score = S, level = L}.

%-------------------------------------------------------------------------------------------------
-spec to_list(lsnode()) -> [term()].

to_list(Anode) when Anode#lsnode.children == [] -> Anode#lsnode.score;
to_list(Anode) -> [?MODULE:to_list(A) || A <- Anode#lsnode.children].

%-------------------------------------------------------------------------------------------------
-spec max(lsnode() | [number()]) -> integer().

max(A) when is_list(A) -> ?MODULE:max(?MODULE:from_list(A));
max(A) -> ?MODULE:map(A, fun lists:max/1).

%-------------------------------------------------------------------------------------------------
-spec min(lsnode() | [number()]) -> integer().

min(A) when is_list(A) -> ?MODULE:min(?MODULE:from_list(A));
min(A) -> ?MODULE:map(A, fun lists:min/1).

%-------------------------------------------------------------------------------------------------
-spec max_min(lsnode() | [number()]) -> integer().

max_min(A) when is_list(A) -> ?MODULE:max_min(?MODULE:from_list(A));
max_min(A) -> ?MODULE:map(A, fun lists:max/1, fun lists:min/1).

%-------------------------------------------------------------------------------------------------
-spec min_max(lsnode() | [number()]) -> integer().

min_max(A) when is_list(A) -> ?MODULE:min_max(?MODULE:from_list(A));
min_max(A) -> ?MODULE:map(A, fun lists:min/1, fun lists:max/1).

%-------------------------------------------------------------------------------------------------
-spec max_min_p(lsnode() | [number()]) -> integer().

max_min_p(A) when is_list(A) -> max_min_p(?MODULE:from_list(A));
max_min_p(A) -> ?MODULE:map(A, fun pmax/3, fun pmin/3, {?BETA_MIN, ?ALPHA_MAX}).

%-------------------------------------------------------------------------------------------------
-spec min_max_p(lsnode() | [number()]) -> integer().

min_max_p(A) when is_list(A) -> min_max_p(?MODULE:from_list(A));
min_max_p(A) -> ?MODULE:map(A, fun pmin/3, fun pmax/3, {?BETA_MIN, ?ALPHA_MAX}).

%-------------------------------------------------------------------------------------------------
-spec map(lsnode(), fun()) -> integer() | [integer()].

map(Anode, _Fn) when Anode#lsnode.children == [] -> Anode#lsnode.score;
map(Anode, Fn) -> Fn([?MODULE:map(A, Fn) || A <- Anode#lsnode.children]).

%-------------------------------------------------------------------------------------------------
-spec map(lsnode(), fun(), fun()) -> integer() | [integer()].

map(Anode, _Fa, _Fb) when Anode#lsnode.children == [] -> Anode#lsnode.score;
map(Anode, Fa, Fb) -> Fa([?MODULE:map(A, Fb, Fa) || A <- Anode#lsnode.children]).

%-------------------------------------------------------------------------------------------------
-spec map(lsnode(), fun(), fun(), {number(), number()}) -> integer() | [integer()].

map(Anode, _Fa, _Fb, _) when Anode#lsnode.children == [] -> Anode#lsnode.score;
map(Anode, Fa, Fb, {Alpha, Beta}) -> 
	Fa( fun(N, {A, B}) -> ?MODULE:map(N, Fb, Fa, {A, B}) end,
		{Alpha, Beta},
		Anode#lsnode.children).

%-------------------------------------------------------------------------------------------------
-spec pmax(fun(), {number(), number()}, [lsnode()]) -> number().
pmax(F, {AL, BE}, Lst) -> pmax(F, {AL, BE}, Lst, ?BETA_MIN). 

-spec pmax(fun(), {number(), number()}, [lsnode()], number()) -> number().
pmax(_F, _, [], X) ->  X;
pmax(F, {Al, Be}, [N | Tail], X) -> 
	A =  erlang:max(F(N, {Al, Be}), X),
	case A < Be of
		true  -> pmax(F, {A, Be}, Tail, A);
		false -> A      % pruning
	end.

%-------------------------------------------------------------------------------------------------
-spec pmin(fun(), {number(), number()}, [lsnode()]) -> number().
pmin(F, {AL, BE}, Lst) -> pmin(F, {AL, BE}, Lst, ?ALPHA_MAX). 

-spec pmin(fun(), {number(), number()}, [lsnode()], number()) -> number().
pmin(_F, _, [], X) ->  X;
pmin(F, {Al, Be}, [N | Tail], X) -> 
	B =  erlang:min(F(N, {Al, Be}), X),
	case Al < B of
		true  -> pmin(F, {Al, B}, Tail, B);
		false -> B      % pruning
	end.

%-------------------------------------------------------------------------------------------------
