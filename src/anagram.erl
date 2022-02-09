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

% -----------------------------------------------------------------------------
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @copyright 2022 Anatoly Rodionov
% @doc
% Checks if two words are are formed by rearaging their letters.
% Not the fastest way to compare. Can be slow for long strings
% because big numbers arithmetic is slow, though
% <i>eqp</i> looks fast enough for me.
% Just for fun - I tried to use only arithmetic operations.
% @end
% -----------------------------------------------------------------------------

- module(anagram).

-export([eq/2, eq/4]).
-export([as_sum/1, as_sum/3]).
-export([as_prod/1, as_prod/2]).
-export([as_prime_prod/1, as_prime_prod/2]).
-export([prime/1]).
-export([eqp/2, eqp/3]).

-define(ASCII_PRINTABLE_SIZE,  96).  % number of printable ASCII characters: 126-31+1
-define(ASCII_PRINTABLE_BASE, ?ASCII_PRINTABLE_SIZE).
-define(ASCII_PRINTABLE_START, 31).  % <i>space</i> (code 32) is mapped to 1

% -----------------------------------------------------------------------------

-spec eq(string(), string()) -> boolean().
% @doc Checks that two strings consists of the same multy set of letters.
% Same as <i>eq(A, B, ?ASCII_PRINTABLE_START, ?ASCII_PRINTABLE_BASE)</i>
eq(A, B) -> eq(A, B, ?ASCII_PRINTABLE_START, ?ASCII_PRINTABLE_BASE).

-spec eq(string(), string(), pos_integer(), pos_integer()) -> boolean().
% @doc Checks that two strings consists of the same multy set of letters.
eq(A, B, Start, Base) -> 
    length(A) == length(B) andalso
    lists:sum(A) == lists:sum(B) andalso       % optimization
    % as_prod(A, Start) == as_prod(B, Start) andalso 
    % I think that previous two checks are sufficient 
    % but was not able to prove this 
    as_sum(A, Start, Base) == as_sum(B, Start, Base).

% -----------------------------------------------------------------------------
-spec as_sum(string()) -> pos_integer().
% @doc Calculates unique integer for string.
% Same as <i>as_sum(A, ?ASCII_PRINTABLE_START, ?ASCII_PRINTABLE_BASE)</i>
as_sum(A) -> as_sum(A, ?ASCII_PRINTABLE_START, ?ASCII_PRINTABLE_BASE).

-spec as_sum(string(), pos_integer(), pos_integer()) -> pos_integer().
% @doc Calculates unique integer for string.
as_sum(A, Start, Base) -> 
    P = Base * length(A),     % to prevent carring over
    lists:foldl(fun(X, S) -> (X-Start)*P + S end, 0, A).

% -----------------------------------------------------------------------------
-spec as_prod(string()) -> pos_integer().
% @doc Calculates product of all charactes
% Same as <i>as_prod(A, ?ASCII_PRINTABLE_START)</i>
as_prod(A) -> as_prod(A, ?ASCII_PRINTABLE_START).

-spec as_prod(string(), pos_integer()) -> pos_integer().
% @doc Calculates product of all charactes
as_prod(A, Start) -> lists:foldl(fun(X, P) -> (X-Start)*P end, 1, A).

% -----------------------------------------------------------------------------
-spec eqp(string(), string()) -> boolean().
% @doc Checks that two strings consists of the same multy set of letters.
% Uses mapping strings to prime number products for comparison
% Same as <i>eqp(A, B, ?ASCII_PRINTABLE_START)</i>
eqp(A, B) -> eqp(A, B, ?ASCII_PRINTABLE_START).

-spec eqp(string(), string(), pos_integer()) -> boolean().
% @doc Checks that two strings consists of the same multy set of letters.
% Uses mapping strings to prime number products for comparison
eqp(A, B, Start) -> 
    length(A) == length(B) andalso       % optimisation
    as_prime_prod(A, Start) == as_prime_prod(B, Start).

% -----------------------------------------------------------------------------
-spec as_prime_prod(string()) -> pos_integer().
% @doc Calculates product of all charactes mapped to its' prime numbers 
% Same as <i>as_prime_prod(A, ?ASCII_PRINTABLE_START)</i>
as_prime_prod(A) -> as_prime_prod(A, ?ASCII_PRINTABLE_START).

-spec as_prime_prod(string(), pos_integer()) -> pos_integer().
% @doc Calculates product of all charactes mapped to its' prime numbers 
as_prime_prod(A, Start) -> lists:foldl(fun(X, P) -> prime(X-Start)*P end, 1, A).

% ----------------------------------------------------------------------------- 
-spec prime(pos_integer()) -> pos_integer().
% @doc Retuns prime number (from first 100).
% Numbers taken from <i>wikpedia</i>
% <a href="https://en.wikipedia.org/wiki/List_of_prime_numbers">
% <i>"The first 100 prime numbers"</i></a>.
prime(N) -> element(N, {
        2,
        3,
        5,
        7,
        11,
        13,
        17,
        19,
        23,
        29,
        31,
        37,
        41,
        43,
        47,
        53,
        59,
        61,
        67,
        71,

        73,
        79,
        83,
        89,
        97,
        101,
        103,
        107,
        109,
        113,
        127,
        131,
        137,
        139,
        149,
        151,
        157,
        163,
        167,
        173,

        179,
        181,
        191,
        193,
        197,
        199,
        211,
        223,
        227,
        229,
        233,
        239,
        241,
        251,
        257,
        263,
        269,
        271,
        277,
        281,

        283,
        293,
        307,
        311,
        313,
        317,
        331,
        337,
        347,
        349,
        353,
        359,
        367,
        373,
        379,
        383,
        389,
        397,
        401,
        409,

        419,
        421,
        431,
        433,
        439,
        443,
        449,
        457,
        461,
        463,
        467,
        479,
        487,
        491,
        499,
        503,
        509,
        521,
        523,
        541
    }).
