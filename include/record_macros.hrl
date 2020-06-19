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
%%% I found these macros useful for debugging and fast prototyping.
%%% You may take a look on simple example in recods_macros.erl in src
%%% directory 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(RECORD_MACROS_HRL).
-define(RECORD_MACROS_HRL, true).

%-----------------------------------------------------------------------------------------------

% @doc Creates anonymous function which returns field index in the record RECORD_NAME.
% @spec fun([atom()], atom(), pos_integer(), fun()) -> pos_integer().
-define(FIND_RECORD_INDEX_BY_NAME, fun
    ([],        _, _, _) -> 1;
    ([X |   _], X, N, _) -> N;
    ([_ | Cdr], X, N, F) -> F(Cdr, X, N + 1, F)
	end).

% @doc Creates one argument function with the name RECORD_NAME which returns
% if the argument is a record returns map with record field name as keys.
% @spec RECORD_NAME(record()) -> map().
% If the function argument is a map then it creates record with name RECORD_NAME
% and fields values of corresponding values in map or undefined if there is no such key.
% @spec RECORD_NAME(map()) -> record().
-define(RECORD_TF_MAP(RECORD_NAME),
    RECORD_NAME(Record) when is_record(Record, RECORD_NAME) -> 
        maps:from_list(lists:zip(record_info(fields, RECORD_NAME), lists:nthtail(1, tuple_to_list(Record))));
    RECORD_NAME(Map) when is_map(Map) ->
        list_to_tuple([RECORD_NAME | [maps:get(X, Map, undefined) || X <- record_info(fields, RECORD_NAME)]])
    ).

% @doc Creates two arguments function with the name RECORD_NAME which returns value
% of the field Name in Record.
% @spec RECORD_NAME(record(), atom()) -> term().
-define(GET_RECORD_FIELD_BY_NAME(RECORD_NAME),
    RECORD_NAME(Record, Name) when is_record(Record, RECORD_NAME) ->
    	F = ?FIND_RECORD_INDEX_BY_NAME,
        element(F(record_info(fields, RECORD_NAME), Name, 2, F), Record)
    ). 

% @doc Creates three arguments function with the name RECORD_NAME which sets value
% of the field Name in Record to Value.
% @spec RECORD_NAME(record(), atom(), term()) -> tuple().
-define(SET_RECORD_FIELD_BY_NAME(RECORD_NAME),
    RECORD_NAME(Record, Name, Value) when is_record(Record, RECORD_NAME) -> 
    	F = ?FIND_RECORD_INDEX_BY_NAME,
        setelement(F(record_info(fields, RECORD_NAME), Name, 2, F), Record, Value)
    ). 

%-----------------------------------------------------------------------------------------------

-endif.
