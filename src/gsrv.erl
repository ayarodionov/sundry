%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
%%% @copyright 2021 Anatoly Rodionov
%%%
%%% @doc This is a simple implementation of gen_server multi call.
%%% <b>Requires Erlang/OTP 24</b>.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(gsrv).

%% API exports
-export([multicall1/1, multicall2/1, multicall3/1, multicall4/1, multicall4/2]).

-define(DEFAULT_TIMEOUT, 5000).  % gen_server:call default timeout

-define(CURRENT_TIME, erlang:system_time(milli_seconds)).

-spec multicall1([{pid(), term()} | {pid(), term(), non_neg_integer() | infinity}]) -> 
  [term()].
% @doc Naive implementation; makes gen_server calls one after another.
% Execution time is proportional to  sum of all call times.
% 
% This not solution. This function is for illustration and comparison.
% 
% @param CallRequests list of call requests
% @returns list of calls responses in the same order as calls
multicall1(CallRequests) ->
    lists:map(fun 
      ({Pid, Call})          -> catch gen_server:call(Pid, Call);
      ({Pid, Call, Timeout}) -> catch gen_server:call(Pid, Call, Timeout)
      end,  CallRequests).

-spec multicall2([{pid(), term()} | {pid(), term(), non_neg_integer() | infinity}]) -> 
  [{reply, term()} | timeout | {error, term()}].
% @doc Makes asynchronous gen_server calls.
% First asynchronously sends all requests using gen_server:send_request. 
% Then gen_server:receive_response receives results.
% In this case calls to different recipients are executed concurrently
% by their servers.
% 
% Execution time may be proportional to time of the longest call.
% Let N be the the CallRequests length and M - number of different servers;
% n(i) - number of calls to server(i), t(i,j) - execution time of 
% call(i, j) to service(i). Here 0 &lt; i &lt;= N, 0 &lt; j &lt;= n(i). 
% Let t(i) be the sum of all t(i,j). Expectation time of multicall2 call
% is evidently max(t(i)).
% 
% In the worst case scenario when N=1 multicall2 is no better than multicall1,
% but when N=M then execution time may be proportional to time of the longest call.
% 
% The other worst case scenario is when all recipients never answer. 
% In this case full execution time is sum of all timeouts.
% 
% @param CallRequests list of call requests
% @returns list of receive_response return values  in the same order as calls
multicall2(CallRequests) ->
    CallRefs = lists:map(fun 
      ({Pid, Call})          -> {gen_server:send_request(Pid, Call), ?DEFAULT_TIMEOUT};
      ({Pid, Call, Timeout}) -> {gen_server:send_request(Pid, Call), Timeout}
      end, CallRequests),
    lists:map(fun ({Ref, Tout}) -> gen_server:receive_response(Ref, Tout) end, CallRefs).

-spec multicall3([{pid(), term()} | {pid(), term(), non_neg_integer() | infinity}]) -> 
  [{reply, term()} | timeout | {error, term()}].
% @doc Same as multicall2 but treats timeouts differently - it
% calculates time for each call starting from the last call.
% 
% Problem of multicall2 is that it changes timeout semantic.
% It interprets timeout as time of the time started from receive_response call,
% not from the time of send_request call. multicall3 fixes this by using time 
% the last send_request as the starting time for each receive_response.
% 
% multicall3 not only fixes timeout semantic but also makes the second worst
% scenario (all recipient do not answer in time). multicall3 will wait only
% maximum of timeout times.
% 
% There is still reason for using multicall2 - it gives you maximum chances to
% to receive answers from recipients.
% 
% @param CallRequests list of call requests
% @returns list of receive_response return values in the same order as calls
multicall3(CallRequests) ->
    CallRefs = lists:map(fun 
      ({Pid, Call})          -> {gen_server:send_request(Pid, Call), ?DEFAULT_TIMEOUT};
      ({Pid, Call, Timeout}) -> {gen_server:send_request(Pid, Call), Timeout}
      end, CallRequests),
    StartTime = ?CURRENT_TIME,
    lists:map(fun 
      ({Ref, infinity}) -> gen_server:receive_response(Ref, infinity);
      ({Ref, Tout})     -> gen_server:receive_response(Ref, max(0, StartTime + Tout - ?CURRENT_TIME))
      end, CallRefs).
% 
-spec multicall4([{pid(), term()}]) -> 
  [{reply, term()} | timeout | {error, term()}].
% @doc Same as multicall4(CallRequests, ?DEFAULT_TIMEOUT)
% @param CallRequests list of call requests
% @returns list of receive_response return values
multicall4(CallRequests) ->  multicall4(CallRequests, ?DEFAULT_TIMEOUT).

-spec multicall4([{pid(), term()}], non_neg_integer() | infinity) -> 
  [{reply, term()} | timeout | {error, term()}].
% @doc Same as multicall2 but another timeout semantic; 
% timeout is understood as time limit for all calls to be completed.
% 
% @param CallRequests list of call requests
% @param Timeout is time limit for all calls to be completed
% @returns list of receive_response return values  in the same order as calls
multicall4(CallRequests, infinity) ->
    CallRefs = [gen_server:send_request(Pid, Call) || {Pid, Call} <- CallRequests],
    [gen_server:receive_response(Ref, infinity) || Ref <- CallRefs];
multicall4(CallRequests, Timeout) ->
    CallRefs = [gen_server:send_request(Pid, Call) || {Pid, Call} <- CallRequests],
    EndTime = ?CURRENT_TIME + Timeout,
    [gen_server:receive_response(Ref, max(0, EndTime - ?CURRENT_TIME)) || Ref <- CallRefs].
