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
%%% @doc collection of macros for working with record field names
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------------------------

-module(record_macros).

-include("../include/record_macros.hrl").

-export([rrr/2]).
-export([rrr/3]).

-export([test/0, test/1, test/2]).

-record(rrr, {a,b,c}).



test() -> rrr(#rrr{a=1,b=2,c=3}).

test(X) -> rrr(#rrr{a=1,b=2,c=3}, X).

test(X,Y) -> rrr(#rrr{a=1,b=2,c=3}, X, Y).

?GET_RECORD_ALL_FIELDS(rrr).
?GET_RECORD_FIELD_BY_NAME(rrr).
?SET_RECORD_FIELD_BY_NAME(rrr).

% record_macros:test(b).

% F = fun(Lst, X, N, F) -> 
% 	case Lst of
% 		[]        -> 1;
% 		[X | _]   -> N;
% 		[_ | Cdr] -> F(Cdr, X, N + 1, F)
% 	end
% end.
