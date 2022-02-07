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
%%% @doc Erlang implementation of longest increasing subsequence algorithm
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lisub).

%-----------------------------------------------------------------------------------------------
%% API exports
-export([ln/1, li/1]).

% -define(TEST,true).

-ifdef(TEST).
-export([max_second/2, max_second/3, max_insert/2, max_insert/3, ns/2, li/2]).
-else.
-compile([{inline, [max_second/2, max_insert/2]}]).
-endif.

%-----------------------------------------------------------------------------------------------
-spec ln([integer()]) -> non_neg_integer().
% @doc n square algorithm
ln(L) -> lists:foldl(fun({_, N}, M) -> max(N, M) end, 0, ns(L, [])).

-spec ns([integer()], [{integer(), non_neg_integer()}]) -> [{integer(), non_neg_integer()}].
ns([], Acc) -> Acc;
ns([A | Tail], Acc) -> ns(Tail, [{A, max_second(A, Acc) + 1} | Acc]).

%-----------------------------------------------------------------------------------------------
-spec max_second(integer(), [{integer(), non_neg_integer()}]) -> non_neg_integer().
max_second(A, Lst) -> max_second(A, Lst, 0).

-spec max_second(integer(), [{integer(), non_neg_integer()}], non_neg_integer()) -> non_neg_integer().
max_second(_, [], M) -> M;
max_second(A, [{B, N} | Tail], M) when A > B andalso N > M -> max_second(A, Tail, N);
max_second(A, [_ | Tail], M)  -> max_second(A, Tail, M).

%-----------------------------------------------------------------------------------------------
-spec li([integer()]) -> non_neg_integer().
% @doc n square algorithm
li(L) -> lists:foldl(fun({_, N}, M) -> max(N, M) end, 0, ni(L, [])).

-spec ni([integer()], [{integer(), non_neg_integer()}]) -> [{integer(), non_neg_integer()}].
ni([], Acc) -> Acc;
ni([A | Tail], Acc) -> ni(Tail, max_insert(A, Acc)).

%-----------------------------------------------------------------------------------------------
-spec max_insert(integer(), [{integer(), non_neg_integer()}]) -> [{integer(), non_neg_integer()}].
max_insert(A, Lst) -> max_insert(A, 0, Lst).

-spec max_insert(integer(), non_neg_integer(), [{integer(), non_neg_integer()}]) -> [{integer(), non_neg_integer()}].
max_insert(A, M, []) -> [{A, M + 1}];
max_insert(A, M, L = [{B, _} | _]) when A < B -> [{A, M + 1} | L];
max_insert(A, M, [{A, N} | Tail]) -> [{A, max(M + 1, N)} | Tail];
max_insert(A, M, [{B, N} | Tail]) -> [{B, N} | max_insert(A, max(M, N), Tail)].

%-----------------------------------------------------------------------------------------------
