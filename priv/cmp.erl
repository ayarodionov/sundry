#!/usr/bin/env escript

% -----------------------------------------------------------------------------
% @doc
% Comparision of different ways to write effective code for operations on lists.
% 
% I compare 
% <ol>
%     <li>lists comprehension</li>
%     <li>calculations using lists: module</li>
%     <li>calculations using recursive functions</li>
% </ol>
% To my surprise the third approach works always better and 
% the second one proved to be the worth 
% 
% -----------------------------------------------------------------------------

-module(cmp).

-mode(compile).

-export([main/1]).

% Tests
-export([filter/3, cnt/3]).

% Testing tools:
-export([mk_example/2, time_tst/3, average_time/3, compare_methods/3, compare_methods/4]).

-compile([{inline, [count/2]}]).

main(_) -> 
    P=3, FLst = [fun(Lst) -> cmp:filter(Lst, P, I) end || I <- [1,2,3]],
    io:format("filter: ~p~n", [cmp:compare_methods(FLst, lists:seq(1,10000000), 10)]),

    R=999, FL = [fun(Lst) -> cmp:cnt(Lst, R, I) end || I <- [1,2,3]],
    io:format("cnt: ~p~n", [cmp:compare_methods(FL, lists:seq(1,10000000), 10)]).
% -----------------------------------------------------------------------------

-spec filter([integer()], integer(), 1 | 2 | 3) -> [integer()].
% @doc Calls different methods for to filter even numbers from a list
% 
% <ol>
%     <li><i>filter(Lst, P, 1)</i> - lists comprehension</li>
%     <li><i>filter(Lst, P, 2)</i> - calls lists:filter</li>
%     <li><i>filter(Lst, P, 3)</i> - by recursive function</li>
% </ol>
% Example:
% <pre>
% Erlang/OTP 24 [erts-12.1.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [dtrace]
% Eshell V12.1.5  (abort with ^G)
% 1> c(cmp).
% {ok,cmp}
% 2> P=3, FLst = [fun(Lst) -> cmp:filter(Lst, P, I) end || I - [1,2,3]].
% 3> cmp:compare_methods(FLst, lists:seq(1,10000000), 10).
% [492005.4,530288.0,329764.3]
% </pre>
% Here
% <ol>
%     <li><i>492005.4</i> - lists comprehension</li>
%     <li><i>530288.0</i> - calls lists:filter</li>
%     <li><i>329764.3</i> - by recursive function</li>
% </ol>
filter(Lst, P, 1) -> filter1(Lst, P);
filter(Lst, P, 2) -> filter2(Lst, P);
filter(Lst, P, 3) -> filter3(Lst, P).

-spec filter1([integer()], integer()) -> [integer()].
% @doc Lists comprehension filter for even numbers
filter1(Lst, P) -> [X || X <- Lst, (X rem P) == 0 ].

-spec filter2([integer()], integer()) -> [integer()].
% @doc Filters even numbers using lists:filter.
filter2(Lst, P) -> lists:filter(fun(X) -> (X rem P) == 0 end, Lst).

-spec filter3([integer()], integer()) -> [integer()].
% @doc Even numbers recursive  filter.
filter3([], _) -> [];
filter3([X | Tail], P) when (X rem P) == 0 -> [X | filter3(Tail, P)];
filter3([_ | Tail], P) -> filter3(Tail, P).

% -----------------------------------------------------------------------------

-spec cnt([integer()], integer(), 1 | 2 | 3) -> {integer(), integer()}.
% @doc Counts number of numbers {Less than  P, Equal to  P}
% 
% <ol>
%     <li><i>cnt(Lst, P, 1)</i> - calls lists:foldl with function inlined</li>
%     <li><i>cnt(Lst, P, 2)</i> - calls lists:foldl</li>
%     <li><i>cnt(Lst, P, 3)</i> - by recursive function</li>
% </ol>
% Example:
% <pre>
% Erlang/OTP 24 [erts-12.1.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [dtrace]
% Eshell V12.1.5  (abort with ^G)
% 1> c(cmp).
% {ok,cmp}
% 2> R=999, FL = [fun(Lst) -> cmp:cnt(Lst, R, I) end || I <- [1,2,3]].
% 3> cmp:compare_methods(FL, lists:seq(1,10000000), 10).
% [264023.2,281910.9,108400.2]
% </pre>
% Here
% <ol>
%     <li><i>264023.2</i> - calls lists:foldl with function inlined</li>
%     <li><i>281910.9</i> - calls lists:foldl</li>
%     <li><i>108400.2</i> - by recursive function</li>
% </ol>
cnt(Lst, P, 1) -> 
    lists:foldl(fun
        (X, {L, E}) when X <  P -> {L + 1, E}; 
        (X, {L, E}) when X == P -> {L, E + 1};
        (_, Acc) -> Acc
    end, {0, 0}, Lst);
cnt(Lst, P, 2) -> lists:foldl(fun(X, A) -> cmpt(X, A, P) end, {0, 0}, Lst);
cnt(Lst, P, 3) -> count(Lst, P).


-spec cmpt(integer(), {integer(), integer()}, integer()) ->  {integer(), integer()}.
cmpt(P, {L, E}, P) -> {L, E + 1};
cmpt(X, {L, E}, P) when X < P -> {L + 1, E};
cmpt(_, A, _) -> A.


-spec count([number()], number()) -> {non_neg_integer(), non_neg_integer()}.
% @doc Counts number of numbers {Less than  P, Equal to  P}. 
count(Lst, P) -> count(Lst, P, {0,0}).

-spec count([number()], number(), {non_neg_integer(), non_neg_integer()}) -> 
    {non_neg_integer(), non_neg_integer()}.
% @doc Counts number of numbers {Less than  P, Equal to  P}. 
count([], _, Acc)            -> Acc;
count([P | Tail], P, {L, E}) -> count(Tail, P, {L, E + 1});
count([X | Tail], P, {L, E}) when X < P -> count(Tail, P, {L + 1, E});
count([_ | Tail], P, Acc)    -> count(Tail, P, Acc).

% -----------------------------------------------------------------------------
% Testing tools
% -----------------------------------------------------------------------------
-spec mk_example(pos_integer(), pos_integer()) -> [pos_integer()].
% @doc Creates example for testing
mk_example(Length, Range) -> [rand:uniform(Range) || _ <- lists:seq(1, Length)].

-spec time_tst(fun(), [number()], pos_integer()) -> [{number(), number()}]. 
% @doc Calls time:tc Repete times
time_tst(Function, Example, Repete) -> 
    [timer:tc(Function, [Example]) ||  _ <- lists:seq(1, Repete)].

-spec average_time(fun(), [number()], pos_integer()) -> number(). 
% @doc Calculates average time of function executtion
% @returns average execution time
average_time(Function, Example, Repete) ->
    L = time_tst(Function, Example, Repete),
    lists:foldl(fun({X, _}, S) -> X+S end, 0,L)/length(L).

-spec compare_methods([fun()], [number()], pos_integer()) -> [{number(), number()}].
% @doc Calculates everage times calling different function on the same example
% @returns list of execution times 
compare_methods(Functions, Example, Repete) ->
    [average_time(F, Example, Repete) || F <- Functions].

-spec compare_methods([fun()], pos_integer(), pos_integer(), pos_integer()) -> 
    [number()].
% @doc Calculates everage times calling different function on the same example
% @returns list of execution times 
compare_methods(Functions, Length, Range, Repete) ->
    compare_methods(Functions, mk_example(Length, Range), Repete).

% -----------------------------------------------------------------------------
