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
% because big numbers arithmetic is slow.
% Just for fun - I tried to use only arithmetic operations.
% @end
% -----------------------------------------------------------------------------

- module(anagram).

-export([eq/2, eq/4]).
-export([as_sum/1, as_sum/3]).
-export([as_prod/1, as_prod/2]).

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
