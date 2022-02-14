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
%%% @doc Collection functions for working with strings
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(sutils).

%-----------------------------------------------------------------------------------------------
%% API exports
-export([lcs/2, lcs_len/2]).

%-----------------------------------------------------------------------------------------------
-spec lcs(list(), list())-> list().
% @doc Finds longest common subsequence.
lcs([], _) -> [];
lcs(_, []) -> [];
lcs([A | TialA], [A | TialB]) -> [A | lcs(TialA, TialB)];
lcs(LA = [_ | TailA], LB = [_ | TailB]) ->
    LscA = lcs(LA, TailB), 
    LscB = lcs(TailA, LB),
    case length(LscA) > length(LscB) of 
        true  -> LscA;
        false -> LscB
    end.

%-----------------------------------------------------------------------------------------------
-spec lcs_len(list(), list()) -> non_neg_integer().
% @doc Finds length of the longest common subsequence.
lcs_len(A, B) -> lcs_len(A, B, 0).

-spec lcs_len(list(), list(), non_neg_integer()) -> non_neg_integer().
% @doc Finds length of the longest common subsequence; internal function
lcs_len([], _, N) -> N;
lcs_len(_, [], N) -> N;
lcs_len([A | TialA], [A | TialB], N) -> lcs_len(TialA, TialB, N + 1);
lcs_len (LA = [_ | TailA], LB = [_ | TailB], N) -> 
    max(lcs_len(LA, TailB, N), lcs_len(TailA, LB, N)).

%-----------------------------------------------------------------------------------------------
