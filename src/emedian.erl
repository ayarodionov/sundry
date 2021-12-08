% -----------------------------------------------------------------------------
% @doc
% Erlang version of calculating median algorithm
% base on RusselL Cohen post:
% <a href="https://rcoh.me/posts/linear-time-median-finding">
% My Favorite Algorithm: Linear Time Median Finding
% </a>
% 
% -----------------------------------------------------------------------------

- module(emedian).

-export([median/1, qselect/2, sort_median/1]).
% -export([qselect/1]).

% For testing:
-export([simple_test/0, mk_example/2, time_tst/3]).
-export([average_time/3, compare_methods/3, compare_methods/4, compare_times/4]).
% For comparing lists:fold with recursive functions
-export([filter/3, filter1/2, filter2/2, filter3/2, cnt/3]).

% -compile(export_all).
-compile([{inline, [pivot/1, count/3, median/3]}]).

% -----------------------------------------------------------------------------

-spec median([number()]) -> number().
% @doc Calculates median of list of numbers.
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
% @doc Calculates median of list of numbers; uses lists:usort.
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
% Testing
% -----------------------------------------------------------------------------
-spec mk_example(pos_integer(), pos_integer()) -> [pos_integer()].
% @doc Creates example for testing
mk_example(Length, Range) -> [rand:uniform(Range) || _ <- lists:seq(1, Length)].

-spec time_tst(fun(), [number()], pos_integer()) -> [{number(), number()}]. 
% @doc Repeates time:tc Repeete times
time_tst(Function, Example, Repete) -> 
    [timer:tc(Function, [Example]) ||  _ <- lists:seq(1, Repete)].

-spec average_time(fun(), [number()], pos_integer()) -> {number(), number()}. 
% @doc Calculates average time of function execution
average_time(Function, Example, Repete) ->
    L = time_tst(Function, Example, Repete),
    {element(2, lists:nth(1, L)), lists:foldl(fun({X, _}, S) -> X+S end, 0,L)/length(L)}.

-spec compare_methods([fun()], [number()], pos_integer()) -> [{number(), number()}].
% @doc Calculates everage times or different functions on the same example
compare_methods(Functions, Example, Repete) ->
    [average_time(F, Example, Repete) || F <- Functions].

-spec compare_methods([fun()], pos_integer(), pos_integer(), pos_integer()) -> 
    [{number(), number()}].
% @doc Calculates everage times or different functions on the same example
compare_methods(Functions, Length, Range, Repete) ->
    compare_methods(Functions, mk_example(Length, Range), Repete).

-spec compare_times([fun()], pos_integer(), pos_integer(), pos_integer()) -> 
    [{number(), number()}].
% @doc Calculates everage times or different functions on the same example
compare_times(Functions, Length, Range, Repete) -> 
    [ T || {_,T} <- compare_methods(Functions, Length, Range, Repete)].

simple_test() ->
    5     = emedian:median([9, 1, 0, 2, 3, 4, 6, 8, 7, 10, 5]),
    5     = emedian:median(lists:seq(1,9)),
    5.5   = emedian:median(lists:seq(1,10)),
    133   = emedian:median(lists:duplicate(17,133)),
    133.0 = emedian:median(lists:duplicate(18,133)),
    4.45  = emedian:median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5]),
    4.4   = emedian:median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3]),
    44    = emedian:median([41, 56, 72, 17, 93, 44, 32]),
    42.5  = emedian:median([41, 72, 17, 93, 44, 32]),
    5     = emedian:sort_median([9, 1, 0, 2, 3, 4, 6, 8, 7, 10, 5]),
    5     = emedian:sort_median(lists:seq(1,9)),
    5.5   = emedian:sort_median(lists:seq(1,10)).

% -----------------------------------------------------------------------------
% emedian:simple_test().
% emedian:time_tst(fun emedian:median/1, emedian:mk_example(11, 4), 5).
% emedian:time_tst(fun emedian:sort_median/1, emedian:mk_example(11, 4), 5).
% emedian:average_time(fun emedian:median/1, emedian:mk_example(11, 4), 5).
% emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], 111111, 41, 10).
% -----------------------------------------------------------------------------
% 2> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], 1111112, 41, 10).
% [{21.0,151735.1},{21.0,436007.9}]
% 3> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], 1111111, 41, 10).
% [{21,140340.4},{21,433576.5}]
% 33> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], 11111111, 41, 10).
% [{21,1540657.4},{21,5052852.6}]
% 34> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [{21.0,1581813.0},{21.0,5397713.2}]
% 38> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], lists:seq(1,11111111), 10).
% [{5555556,2187998.7},{5555556,433697.8}]
% 41> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], lists:seq(1,11111112), 10).
% [{5555556.5,2499763.4},{5555556.5,515321.9}]
% 43> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], lists:duplicate(17,11111112), 10).
% [{11111112,2.3},{11111112,1.5}]
% 44> emedian:compare_methods([fun emedian:median/1, fun emedian:sort_median/1], lists:duplicate(17,11111111), 10).
% [{11111111,1.6},{11111111,1.5}]
% -----------------------------------------------------------------------------
% 11> fprof:apply(emedian, median, [emedian:mk_example(11111, 49)]).
% 26
% 12> fprof:profile().
% Reading trace data...
% ..................................................
% ..............................
% End of trace!
% ok
% 10> fprof:analyse().
% Processing data...
% Creating output...
%% Analysis results:

% Dump results to fprof.analysis file
% fprof:analyse({dest, []}).

% The CNT column shows the total number of function calls that was found in the trace. 
% In the ACC column is the total time of the trace from first timestamp to last. 
% And in the OWN column is the sum of the execution time in functions found in the trace, not including called functions.
% -----------------------------------------------------------------------------
% E = emedian:mk_example(111111, 49).
% F = fun() -> emedian:median(E) end.
% eprof:profile(F).
% eprof:log("qkmedian.eprof.log").
% eprof:analyze().
% -----------------------------------------------------------------------------
% cprof:start(emedian, count).
% cprof:start(emedian, median).
% cprof:start(emedian, qselect).
% F().
% cprof:analyze().
% -----------------------------------------------------------------------------

-spec filter([integer()], integer(), 1 | 2 | 3) -> [integer()].
filter(Lst, P, 1) -> filter1(Lst, P);
filter(Lst, P, 2) -> filter2(Lst, P);
filter(Lst, P, 3) -> filter3(Lst, P).

-spec filter1([integer()], integer()) -> [integer()].
% @doc lists comprehension filter
filter1(Lst, P) -> [X || X <- Lst, (X rem P) == 0 ].

-spec filter2([integer()], integer()) -> [integer()].
% @doc lists.filter  filter
filter2(Lst, P) -> lists:filter(fun(X) -> (X rem P) == 0 end, Lst).

-spec filter3([integer()], integer()) -> [integer()].
% @doc recursive  filter
filter3([], _) -> [];
filter3([X | Tail], P) when (X rem P) == 0 -> [X | filter3(Tail, P)];
filter3([_ | Tail], P) -> filter3(Tail, P).

% 2> P=3, FLst = [fun(Lst) -> emedian:filter(Lst, P, I) end || I <- [1,2,3]].
% [#Fun<erl_eval.44.65746770>,#Fun<erl_eval.44.65746770>,
%  #Fun<erl_eval.44.65746770>]
% 12> emedian:compare_methods(FLst, lists:seq(1,10000000), 10).
% [{[3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,
%    63,66,69,72,75,78,81|...],
%   264723.6},
%  {[3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,
%    63,66,69,72,75,78|...],
%   509415.1},
%  {[3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,
%    63,66,69,72,75|...],
%   278204.9}]

% BIG SUPRISE: lists comprehension filter is better than lists.filter in TWO times!!

% -----------------------------------------------------------------------------

-spec cnt([integer()], integer(), 1 | 2 | 3) -> {integer(), integer()}.
cnt(Lst, P, 1) -> count(Lst, P, 16#7FFFFFF);
cnt(Lst, P, 2) -> 
    lists:foldl(fun
        (X, {L, E}) when X <  P -> {L + 1, E}; 
        (X, {L, E}) when X == P -> {L, E + 1};
        (_, Acc) -> Acc
    end, {0, 0}, Lst);
cnt(Lst, P, 3) -> lists:foldl(fun(X, A) -> cmpt(X, A, P) end, {0, 0}, Lst).  


-spec cmpt(integer(), {integer(), integer()}, integer()) ->  {integer(), integer()}.
cmpt(P, {L, E}, P) -> {L, E + 1};
cmpt(X, {L, E}, P) when X < P -> {L + 1, E};
cmpt(_, A, _) -> A.


% 7> f(R), R=999, f(FL), FL = [fun(Lst) -> emedian:cnt(Lst, R, I) end || I <- [1,2,3]].
% [#Fun<erl_eval.44.65746770>,#Fun<erl_eval.44.65746770>,
%  #Fun<erl_eval.44.65746770>]
% 8> emedian:compare_methods(FL, lists:seq(1,10000000), 10).
% [{{998,1},102174.0},{{998,1},255991.9},{{998,1},270380.8}]

% Another BIG SUPRISE - lists:fold works more than two times slowere than recursive function !

% -----------------------------------------------------------------------------
% Comparing Different Erlang versions:
% row 1 contains times of: emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111111, 41, 10).
% row 2 contains times of: emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).

% ---------------------------------------------------------------------------------------------------------------------------------------
%   | Erlang/OTP 22 [erts-10.7.2.3] | Erlang/OTP 23 [erts-11.2.2.8] | Erlang/OTP 24 [erts-12.1.5]   | Erlang/OTP 24 [erts-12.1.5] [jit] |
% ---------------------------------------------------------------------------------------------------------------------------------------
% 1 | [3196013.3,10050282.7]        | [3345448.7,9124685.2]         | [3300804.0,9686400.1]         | [2231285.7,9193811.0]             |
% ---------------------------------------------------------------------------------------------------------------------------------------
% 2 | [3600751.0,8921677.9]         | [3386780.0,9198170.2]         | [3393113.8,8403296.4]         | [2279661.4,9293728.4]             |        
% ---------------------------------------------------------------------------------------------------------------------------------------


% [arodionov@vaashdmon01 ~]$ erl
% Erlang/OTP 24 [erts-12.1.5] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]

% Eshell V12.1.5  (abort with ^G)
% 1> c(emedian).
% {ok,emedian}
% 2> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111111, 41, 10).
% [2913680.6,9641263.8]
% 3> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [3022733.3,8402577.6]
% 4> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111111, 41, 10).
% [2231285.7,9193811.0]
% 5> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [3418965.4,9395029.0]
% 6> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [2279661.4,9293728.4]
% 7> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [3132527.9,8944623.3]
% 8> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [3516307.4,9686662.1]
% 9> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [3182199.0,9196456.9]
% 10> emedian:compare_times([fun emedian:median/1, fun emedian:sort_median/1], 11111112, 41, 10).
% [2578991.0,9801582.3]


% -----------------------------------------------------------------------------
% CPU Information:
% -----------------------------------------------------------------------------
% lscpu
% Architecture:          x86_64
% CPU op-mode(s):        32-bit, 64-bit
% Byte Order:            Little Endian
% CPU(s):                2
% On-line CPU(s) list:   0,1
% Thread(s) per core:    1
% Core(s) per socket:    1
% Socket(s):             2
% NUMA node(s):          1
% Vendor ID:             GenuineIntel
% CPU family:            6
% Model:                 63
% Model name:            Intel(R) Xeon(R) CPU E5-2650 v3 @ 2.30GHz
% Stepping:              2
% CPU MHz:               2299.998
% BogoMIPS:              4599.99
% Hypervisor vendor:     VMware
% Virtualization type:   full
% L1d cache:             32K
% L1i cache:             32K
% L2 cache:              256K
% L3 cache:              25600K
% NUMA node0 CPU(s):     0,1
% -----------------------------------------------------------------------------


% yum install centos-release-scl -y
% yum clean all
% yum install devtoolset-9-* -y
% scl enable devtoolset-9 bash

% Run 
%   scl enable devtoolset-9 bash
% before 
%   evm install 24.1.6