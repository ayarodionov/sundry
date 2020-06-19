%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2020 Anatoly Rodionov.  All Rights Reserved.
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
%%% @copyright 2020 Anatoly Rodionov
%%%
%%% @doc tests for lstree
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------------------------------------------------------------------------------------

-module(lstree_tests).

%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------

-record(lsnode,{
    level    = 0      :: non_neg_integer(),
    score    = false  :: integer() | false,
    children = []     :: [lsnode()]   
    }).
-type lsnode()       :: #lsnode{}. 

%-----------------------------------------------------------------------------------------------

mk_test() ->
    L = example(0),
	T = {lsnode,0,false,
        [{lsnode,1,1,[]},
         {lsnode,1,2,[]},
         {lsnode,1,3,[]},
         {lsnode,1,false,
                 [{lsnode,2,4,[]},{lsnode,2,false,[{lsnode,3,-5,[]}]}]},
         {lsnode,1,6,[]}]},
	?assertMatch(T, lstree:from_list(L)),
	?assertMatch(L, lstree:to_list(T)),
    ok.

%-----------------------------------------------------------------------------------------------

max_test() ->
    T0 = lstree:from_list(example(0)),
    ?assertMatch( 6, lstree:max(T0)),
    ?assertMatch(-5, lstree:min(T0)),

    T1 = lstree:from_list(example(1)),
    ?assertMatch( 9, lstree:max(T1)),
    ?assertMatch( 3, lstree:min(T1)),

    T2 = lstree:from_list(example(2)),
    ?assertMatch(25, lstree:max(T2)),
    ?assertMatch( 0, lstree:min(T2)),

    T3 = lstree:from_list(example(3)),
    ?assertMatch( 9, lstree:max(T3)),
    ?assertMatch( 1, lstree:min(T3)),
    ok.

%-----------------------------------------------------------------------------------------------

max_min_test() ->
    ?assertMatch( 6, lstree:max_min(example(0))),
    ?assertMatch( 1, lstree:min_max(example(0))),

    T1 = lstree:from_list(example(1)),
    ?assertMatch( 6, lstree:max_min(T1)),
    ?assertMatch( 6, lstree:min_max(T1)),

    T2 = lstree:from_list(example(2)),
    ?assertMatch( 3, lstree:max_min(T2)),
    ?assertMatch(14, lstree:min_max(T2)),

    T3 = lstree:from_list(example(3)),
    ?assertMatch( 5, lstree:max_min(T3)),
    ?assertMatch( 3, lstree:min_max(T3)),

    ok.

%-----------------------------------------------------------------------------------------------

max_min_p_test() ->
    ?assertMatch( 6, lstree:max_min(example(0))),
    ?assertMatch( 1, lstree:min_max(example(0))),

    T1 = lstree:from_list(example(1)),
    ?assertMatch( 6, lstree:max_min_p(T1)),
    ?assertMatch( 6, lstree:min_max_p(T1)),

    T2 = lstree:from_list(example(2)),
    ?assertMatch( 3, lstree:max_min_p(T2)),
    ?assertMatch(14, lstree:min_max_p(T2)),

    T3 = lstree:from_list(example(3)),
    ?assertMatch( 5, lstree:max_min_p(T3)),
    ?assertMatch( 3, lstree:min_max_p(T3)),

    ok.

%-----------------------------------------------------------------------------------------------

example(0) -> [1, 2, 3, [4, [-5]], 6];

example(1) ->
[
    [
        [
            [5,6],
            [7,4,5]
        ],
        [
            [3]
        ]
    ],
    [
        [
            [6],
            [6,9]
        ],
        [
            [7]
        ]
    ],
    [
        [
            [5]
        ],
        [
            [9,8],
            [6]
        ]
    ]
]; 

% example from http://web.cs.ucla.edu/~rosen/161/notes/alphabeta.html
example(2) ->
[
  [[[3, 17], [2, 12]], [[15],  [25, 0]]],
  [[[2, 5], [3]], [[2, 14]]]
];

% example from http://people.cs.pitt.edu/~litman/courses/cs2710/lectures/pruningReview.pdf
example(3) ->
[
    [[4, 3], [6, 2]],
    [[2, 1], [9, 5], [3, 1]],
    [[5, 4], [7, 5]]
].

%-------------------------------------------------------------------------------------------------
