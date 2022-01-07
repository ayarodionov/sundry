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
    length(A) == length(B) andalso       % optimoisation
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
prime(  1) ->	2;
prime(  2) ->	3;
prime(  3) ->	5;
prime(  4) ->	7;
prime(  5) ->	11;
prime(  6) ->	13;
prime(  7) ->	17;
prime(  8) ->	19;
prime(  9) ->	23;
prime( 10) ->	29;
prime( 11) ->	31;
prime( 12) ->	37;
prime( 13) ->	41;
prime( 14) ->	43;
prime( 15) ->	47;
prime( 16) ->	53;
prime( 17) ->	59;
prime( 18) ->	61;
prime( 19) ->	67;
prime( 20) ->	71;

prime( 21) ->	73;
prime( 22) ->	79;
prime( 23) ->	83;
prime( 24) ->	89;
prime( 25) ->	97;
prime( 26) ->	101;
prime( 27) ->	103;
prime( 28) ->	107;
prime( 29) ->	109;
prime( 30) ->	113;
prime( 31) ->	127;
prime( 32) ->	131;
prime( 33) ->	137;
prime( 34) ->	139;
prime( 35) ->	149;
prime( 36) ->	151;
prime( 37) ->	157;
prime( 38) ->	163;
prime( 39) ->	167;
prime( 40) ->	173;

prime( 41) ->	179;
prime( 42) ->	181;
prime( 43) ->	191;
prime( 44) ->	193;
prime( 45) ->	197;
prime( 46) ->	199;
prime( 47) ->	211;
prime( 48) ->	223;
prime( 49) ->	227;
prime( 50) ->	229;
prime( 51) ->	233;
prime( 52) ->	239;
prime( 53) ->	241;
prime( 54) ->	251;
prime( 55) ->	257;
prime( 56) ->	263;
prime( 57) ->	269;
prime( 58) ->	271;
prime( 59) ->	277;
prime( 60) ->	281;

prime( 61) ->	283;
prime( 62) ->	293;
prime( 63) ->	307;
prime( 64) ->	311;
prime( 65) ->	313;
prime( 66) ->	317;
prime( 67) ->	331;
prime( 68) ->	337;
prime( 69) ->	347;
prime( 70) ->	349;
prime( 71) ->	353;
prime( 72) ->	359;
prime( 73) ->	367;
prime( 74) ->	373;
prime( 75) ->	379;
prime( 76) ->	383;
prime( 77) ->	389;
prime( 78) ->	397;
prime( 79) ->	401;
prime( 80) ->	409;

prime( 81) ->	419;
prime( 82) ->	421;
prime( 83) ->	431;
prime( 84) ->	433;
prime( 85) ->	439;
prime( 86) ->	443;
prime( 87) ->	449;
prime( 88) ->	457;
prime( 89) ->	461;
prime( 90) ->	463;
prime( 91) ->	467;
prime( 92) ->	479;
prime( 93) ->	487;
prime( 94) ->	491;
prime( 95) ->	499;
prime( 96) ->	503;
prime( 97) ->	509;
prime( 98) ->	521;
prime( 99) ->	523;
prime(100) ->	541.
 
