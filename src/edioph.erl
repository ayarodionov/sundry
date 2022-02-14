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
%%% @doc Collection of some arithmetic functions absent in Erlang libraries
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(edioph).

%-----------------------------------------------------------------------------------------------
%% API exports
-export([gcd/2, lcm/2, egcd/2, fib/1]).

-compile({inline, ['_lcm'/2]}).

%-----------------------------------------------------------------------------------------------
-spec gcd(integer(), integer())-> non_neg_integer().
% @doc Greatest common divisor
gcd(A, B) when A < 0 -> gcd(-A, B);
gcd(A, B) when B < 0 -> gcd(A, -B);
gcd(A, B) when A >= 0 andalso B >= 0 -> 
    case A > B of true -> '_gcd'(A, B); false -> '_gcd'(B, A) end.

-spec '_gcd'(non_neg_integer(), non_neg_integer())-> non_neg_integer().
% @doc Greatest common divisor; internal function
'_gcd'(A, 0) -> A;
'_gcd'(A, B) -> '_gcd'(B, A rem B). 

%-----------------------------------------------------------------------------------------------
-spec lcm(integer(), integer())-> non_neg_integer().
% @doc  Least common multiple
lcm(A, B) -> '_lcm'(abs(A), abs(B)).

-spec '_lcm'(non_neg_integer(), non_neg_integer())-> non_neg_integer().
% @doc  Least common multiple; internal function
'_lcm'(A, B) when A > B -> (A div '_gcd'(A, B)) * B;
'_lcm'(A, B)            -> (B div '_gcd'(B, A)) * A.

%-----------------------------------------------------------------------------------------------
-spec egcd(integer(), integer()) -> {non_neg_integer(), integer(), integer()}.
% @doc Extended Euclidean algorithm
egcd(A, 0) -> {A, 1, 0};
egcd(A, B) -> 
    {G, X, Y} = egcd(B, A rem B),
    {G, Y, X - Y*(A div B)}.

%-----------------------------------------------------------------------------------------------
-spec fib(non_neg_integer()) -> non_neg_integer().
% @doc Calculates Fibonacci N-th number
fib(N) -> fib(N, 1, 0).

-spec fib(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
% @doc Calculates Fibonacci N-th number; internal function
fib(0, _Fn, Fn1) -> Fn1;
fib(N,  Fn, Fn1) -> fib(N - 1, Fn + Fn1, Fn).

%-----------------------------------------------------------------------------------------------
