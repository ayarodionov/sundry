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

-ifndef(RECORD_MACROS_HRL).
-define(RECORD_MACROS_HRL, true).

%-----------------------------------------------------------------------------------------------

% @doc Creates unanimous function which returns field index in the record RECORD_NAME.
% @spec fun([atom()], atom(), pos_integer(), fun()) -> pos_integer().
-define(FIND_RECORD_INDEX_BY_NAME(RECORD_NAME),
	fun(Lst, X, N, F) -> 
		case Lst of
			[]        -> 1;
			[X | _]   -> N;
			[_ | Cdr] -> F(Cdr, X, N + 1, F)
		end
	end).

% @doc Creates one argument function with the name RECORD_NAME which returns
% key value pairs for all record fields.
% @spec RECORD_NAME(record()) -> [{atom(), term()].
-define(GET_RECORD_ALL_FIELDS(RECORD_NAME),
    RECORD_NAME(Record) when is_record(Record, RECORD_NAME) -> 
        lists:zip(record_info(fields, RECORD_NAME), lists:nthtail(1, tuple_to_list(Record)))).

% @doc Creates two arguments function with the name RECORD_NAME which returns value
% of the field Name in Record.
% @spec RECORD_NAME(record(), atom()) -> term().
-define(GET_RECORD_FIELD_BY_NAME(RECORD_NAME),
    RECORD_NAME(Record, Name) when is_record(Record, RECORD_NAME) ->
    	F = ?FIND_RECORD_INDEX_BY_NAME(RECORD_NAME),
        element(F(record_info(fields, RECORD_NAME), Name, 2, F), Record)). 

% @doc Creates three arguments function with the name RECORD_NAME which sets value
% of the field Name in Record to Value.
% @spec RECORD_NAME(record(), atom(), term()) -> tuple().
-define(SET_RECORD_FIELD_BY_NAME(RECORD_NAME),
    RECORD_NAME(Record, Name, Value) when is_record(Record, RECORD_NAME) -> 
    	F = ?FIND_RECORD_INDEX_BY_NAME(RECORD_NAME),
        setelement(F(record_info(fields, RECORD_NAME), Name, 2, F), Record, Value)). 

%-----------------------------------------------------------------------------------------------

-endif.
