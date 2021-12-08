%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2021 Anatoly Rodionov.  All Rights Reserved.
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
% @copyright 2021 Anatoly Rodionov
% @doc
% Erlang version of calculating median algorithm
% based on <i>RusselL Cohen</i> post:
% <a href="https://rcoh.me/posts/linear-time-median-finding">
% "My Favorite Algorithm: Linear Time Median Finding"
% </a>
% @end
% -----------------------------------------------------------------------------

- module(emedian).

-export([median/1, sort_median/1]).

-compile([{inline, [pivot/1, count/3, median/3]}]).

% -----------------------------------------------------------------------------

-spec median([number()]) -> number().
% @doc Calculates list of numbers median .
median(Lst) -> 
    L = length(Lst),
    LD2 = L div 2,
    case L rem 2 of
        1 -> qselect(Lst, LD2);
        0 -> 
            F = qselect(Lst, LD2 - 1),
            S = median(Lst, F, LD2),
            (F + S )/2 
    end.

-spec median([number()], number(), pos_integer()) -> number().
% @doc Returns F if number of element of Lst which is less or equal to F 
% is greater than L or minimum of elements which are greater then F.
median(Lst, F, L) -> median(Lst, F, L, 0).

-spec median([number()], number(), pos_integer(), non_neg_integer()) -> number().
% @doc Returns F if Cnt - number of element of Lst which is less or equal to F,
% is greater than L or minimum of elements which are greater then F.
median(_, F, L, Cnt)          when Cnt > L -> F;
median([], F, _, _)                        -> F;
median([A | Tail], F, L, Cnt) when A =< F  -> median(Tail, F, L, Cnt+1);
median([A | Tail], F, L, Cnt)              -> median(Tail, F, L, Cnt, A).  % A > F

-spec median([number()], number(), pos_integer(), non_neg_integer(), number()) -> number().
% @doc Returns F if Cnt  - number of element of Lst which is less or equal to F, 
% is greater than L or M - minimum of elements which are greater then F.
median(_, F, L, Cnt, _M)         when Cnt > L -> F;
median([], _, _, _, M)                        -> M;
median([A | Tail], F, L, Cnt, M) when A =< F  -> median(Tail, F, L, Cnt+1, M);
median([A | Tail], F, L, Cnt, M) when A < M   -> median(Tail, F, L, Cnt, A); % A > F;
median([_ | Tail], F, L, Cnt, M)              -> median(Tail, F, L, Cnt, M). % A > F; A >= M

-spec qselect([number()], non_neg_integer()) -> number().
qselect([X], 0) -> X;
qselect(Lst, K) ->
    P = pivot(Lst),
    {Llen, Elen} = count(Lst, P, K),
    case K < Llen of
        true  -> qselect([X || X <- Lst, X < P], K);
        false ->
            Len = Llen + Elen,
            case K < Len of
                true  -> P;
                false -> qselect([X || X <- Lst, X > P], K - Len)
            end
    end.

-spec pivot([term()]) -> term().
% @doc Randomly selects an element from Lst
pivot(Lst) -> lists:nth(rand:uniform(length(Lst)), Lst).

-spec count([number()], number(), non_neg_integer()) -> 
    {non_neg_integer(), non_neg_integer()}.
% @doc Counts number of elements {Less than  P, Equal to  P}. 
count(Lst, P, K) -> count(Lst, P, K, 0, 0).

-spec count([number()], number(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> 
    {non_neg_integer(), non_neg_integer()}.
% @doc Counts number of elements {Less than  P, Equal to  P}. 
count([], _, _, L, E)                    -> {L, E};
count(_, _, K, L, E)          when L > K -> {L, E};
count([P | Tail], P, K, L, E)            -> count(Tail, P, K, L, E + 1);
count([X | Tail], P, K, L, E) when X < P -> count(Tail, P, K, L + 1, E);
count([_ | Tail], P, K, L, E)            -> count(Tail, P, K, L, E).

% -----------------------------------------------------------------------------

-spec sort_median([number()]) -> number().
% @doc Calculates list of numbers median; uses lists:sort.
% Added fot comparing with emedian.
sort_median(Lst) ->
    S = lists:sort(Lst),
    L = length(S),
    M = L div 2,
    case L rem 2 of
        1 -> lists:nth(M+1, S);
        0 -> (lists:nth(M, S) + lists:nth(M+1, S))/2
    end.

% -----------------------------------------------------------------------------
