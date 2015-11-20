%% @copyright 2015 Chef, Inc. All Rights Reserved
%% @author Mark Anderson <mark@chef.io>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.
-module(bksw_sql).

-include("internal.hrl").

%-ifdef(namespaced_types). %% workaround for sqerl complaint
-type dict() :: dict:dict().
%-endif.

-export([ statements/1,
          ping/0,
          create_bucket/1,
          delete_bucket/1,
          find_bucket/1,
          list_buckets/0,
          list_bucket/1,
          create_file/2,
          find_file/2,
          delete_file/2,
          add_file_chunk/3,
          mark_file_done/4,
          get_chunk_data/2
        ]).

-include_lib("sqerl/include/sqerl.hrl").

ping() ->
    case sqerl:select(ping, [], first_as_scalar, [ping]) of
        {ok, <<"pong">>} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc bookshelf queries.
%% Referenced by sys.config in {sqerl, [{ prepared_staements...
statements(_) ->
    Path = filename:join([code:priv_dir(bookshelf), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.

create_bucket(BucketName) ->
    case sqerl:statement(insert_bucket, [BucketName], count) of
        {ok, 1} ->
            ok;
        {conflict, _} ->
            error("Never should happen");
        Error ->
            Error
    end.

delete_bucket(BucketName) ->
    case sqerl:statement(delete_bucket, [BucketName], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

find_bucket(BucketName) ->
    case sqerl:select(find_bucket, [BucketName], first_as_scalar, [bucket_id]) of
        {ok, BucketId} ->
            {ok, BucketId};
        {error, Reason} ->
            {error, Reason}
    end.

list_buckets() ->
    case sqerl:select(list_buckets, [], rows_as_scalars, [bucket_name]) of
        {ok, Buckets} ->
            Buckets;
        {error, Reason} ->
            {error, Reason}
    end.

list_bucket(BucketName) ->
    case sqerl:select(list_bucket_with_hash, [BucketName], rows_as_records, ?DB_FILE_TX_FM) of
        {ok, Buckets} ->
            Buckets;
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_file(binary(), binary()) -> integer().
create_file(Bucket, Name) ->
    case sqerl:select(insert_file, [Bucket, Name], first_as_scalar, [create_file]) of
        {ok, File} ->
            {ok, File};
        {error, Reason} ->
            {error, Reason}
    end.

find_file(Bucket, Name) ->
    case sqerl:select(find_file, [Bucket, Name], {first_as_record, ?DB_FILE_TX_FM }) of
        %% Awkward sanity check that we got back the expected record type here.
        {ok, Object} when db_file =:= element(1, Object) ->
            {ok, Object};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

%% TODO IMPROVE
delete_file(Bucket, Name) ->
    case find_file(Bucket, Name) of
        {ok, none} ->
            {error, file_not_found};
        {ok, #db_file{bucket_id = BucketId}} ->
            case sqerl:statement(delete_file, [BucketId, Name], count) of
                {ok, 1} ->
                    ok;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec add_file_chunk(integer(), integer(), binary()) -> ok.
add_file_chunk(DataId, Sequence, Chunk) ->
    case sqerl:statement(add_file_chunk, [DataId, Sequence, Chunk], count) of
        {ok, 1} ->
            ok;
        {conflict, _} ->
            error("Never should happen");
        Error ->
            Error
    end.


mark_file_done(DataId, Chunks, SumMD5, SumSha512) ->
    case sqerl:statement(update_file_data_done, [DataId, Chunks, SumMD5, SumSha512], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

get_chunk_data(Id, ChunkSequence) ->
    case sqerl:statement(fetch_file_chunk_data, [Id, ChunkSequence], first_as_scalar, [data]) of
        {ok, none} ->
            {ok, not_found};
        {ok, Data} ->
            {ok, Data};
        {error, Error} ->
            {error, Error}
    end.
