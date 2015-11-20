%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% Copyright 2012-2013 Opscode, Inc. All Rights Reserved.
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
%%

-module(bksw_wm_object).

-include_lib("mixer/include/mixer.hrl").
-mixin([{bksw_wm_base, [init/1,
                        is_authorized/2,
                        finish_request/2,
                        service_available/2]}]).

%% Webmachine callbacks
-export([allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         generate_etag/2,
         last_modified/2,
         resource_exists/2,

         %% Override
         validate_content_checksum/2,

         %% Resource helpers
         download/2,
         upload/2]).


-include_lib("webmachine/include/webmachine.hrl").
-include("internal.hrl").

%%===================================================================
%% Public API
%%===================================================================

%% By default, if wm sees a 'content-md5' header, it will read the request body to compute
%% the md5 and compare to the header value. A 400 will then be returned automagically by wm
%% if the digests do not match. Since we wish to read request bodies in a streaming fashion,
%% we need to handle our own checksum validation. Using wm's default would mean having a
%% full copy of the request body buffered into the request process state. So we define this
%% resource callback to blindly say the content is valid and then do the verification in the
%% upload/2 flow.
validate_content_checksum(Rq, Ctx) ->
    {true, Rq, Ctx}.

allowed_methods(Rq, Ctx) ->
    {['HEAD', 'GET', 'PUT', 'DELETE'], Rq, Ctx}.

content_types_provided(Rq, Ctx) ->
    CType =
    case wrq:get_req_header("accept", Rq) of
        undefined ->
            "application/octet-stream";
        "*/*" ->
            "application/octet-stream";
        C ->
            C
    end,
    {[{CType, download}], Rq, Ctx}.

content_types_accepted(Rq, Ctx) ->
    CT = case wrq:get_req_header("content-type", Rq) of
             undefined ->
                 "application/octet-stream";
             X ->
                 X
         end,
    {MT, _Params} = webmachine_util:media_type_to_detail(CT),
    {[{MT, upload}], Rq, Ctx}.

resource_exists(Rq0, Ctx) ->
    case wrq:method(Rq0) of
        %% Buckets always exist for writes since we create them on the fly
        'PUT' ->
            {true, Rq0, Ctx};
        _ ->
            %% determine if the entry exists by opening it. This way, we can cache the fd
            %% and avoid extra system calls. It also helps to keep the request processing
            %% more consistent since we will open the fd once at start and hold on to it.
            %% Note that there is still a possible discrepency when we read the meta data.
            case fetch_entry_md(Rq0, Ctx) of
                {error, none} ->
                    {false, Rq0, Ctx};
                {#db_file{}, CtxNew} ->
                    {true, Rq0, CtxNew}
            end
    end.

last_modified(Rq0, Ctx) ->
    case fetch_entry_md(Rq0, Ctx) of
        {#db_file{created_at = Date}, CtxNew} ->
            {Date, Rq0, CtxNew};
        _ ->
            {halt, Rq0, Ctx}
    end.

generate_etag(Rq0, Ctx) ->
    case fetch_entry_md(Rq0, Ctx) of
        {#db_file{hash_md5 = Digest}, CtxNew} ->
            {bksw_format:to_base64(Digest), Rq0, CtxNew};
        _ ->
            {halt, Rq0, Ctx}
    end.

delete_resource(Rq0, Ctx) ->
    case fetch_entry_md(Rq0, Ctx) of
        {#db_file{bucket_name = BucketName, name = Name}, CtxNew} ->
            %% Note delete file should be changed to use bucket id and be simpler.
            case bksw_sql:delete_file(BucketName, Name) of
                    {ok, 1} ->
                    {true, Rq0, CtxNew};
                {ok, none} ->
                        {halt, Rq0, CtxNew}
            end;
        _ ->
            {halt, Rq0, Ctx}
    end.

%% Return `{Obj, CtxNew}' where `Obj' is the entry meta data `#object{}' record or the atom
%% `error'. The `CtxNew' may have been updated and should be kept. Accessing entry md
%% through this function ensures we only ever read the md from the file system once.
fetch_entry_md(_Req, #context{entry_md = #db_file{} = Obj} = Ctx) ->
    {Obj, Ctx};
fetch_entry_md(Req, #context{} = Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Req),
    case bksw:find_file(Bucket,Path) of
        {ok, #db_file{} = Object} ->
            {Object, Ctx#context{entry_md = Object}};
        {ok, none} ->
            {error, Ctx}
            %% error case here TODO
    end.

%%
%% Resource Helpers
%%

download(Rq0, #context{entry_md = Ref, stream_download = true} = Ctx) ->
    {{stream, send_streamed_body(Ref)}, Rq0, Ctx};
download(Rq0, #context{entry_md = Ref, stream_download = false} = Ctx) ->
    {fully_read(Ref, []), Rq0, Ctx}.

upload(Rq0, Ctx) ->
    {#db_file{}, #context{} = Ctx} = fetch_entry_md(Rq0, Ctx),
    Resp = write_streamed_body(wrq:stream_req_body(Rq0, ?BLOCK_SIZE), Rq0, Ctx),
    Resp.


%%===================================================================
%% Internal Functions
%%===================================================================

send_streamed_body(#context{entry_md = #db_file{chunk_count = ChunkCount},
                            next_chunk_to_stream = ChunkCount}) ->
    {<<>>, done};
send_streamed_body(#context{entry_md = #db_file{data_id = DataId,
                                                chunk_count = ChunkCount},
                            next_chunk_to_stream = ChunkId} = Ctx) ->
    case bksw_sql:get_chunk_data(DataId, ChunkCount) of
        {ok, none} ->
            error_logger:error_msg("Error occurred during content download: ~p~n", [missing_chunk]),
            {error, missing_chunk};
        {ok, Data} ->
            {Data, fun() -> send_streamed_body(Ctx#context{next_chunk_to_stream = ChunkId + 1}) end};
        {error, _} = Error ->
            error_logger:error_msg("Error occurred during content download: ~p~n", [Error]),
            Error
    end.

fully_read({_, done}, Accum) ->
    lists:reverse(Accum);
fully_read({error, Error}, Accum) ->
    error_logger:error_msg("Error occurred during content download: ~p~n", [Error]),
    lists:reverse(Accum);
fully_read({Data, Next}, Accum) ->
    fully_read(Next(), [Data | Accum]).

write_streamed_body({_, done}, Rq0,
                    #context{entry_md = #db_file{data_id = DataId},
                             next_chunk_to_stream = ChunkId} = Ctx ) ->
    HashMd5 = <<"">>,
    HashSha512 = <<"">>,

    bksw_sql:mark_file_done(DataId, ChunkId, HashMd5, HashSha512),

    case get_header('Content-MD5', Rq0) of
        undefined ->
            Rq1 = bksw_req:with_etag(base64:encode(HashMd5), Rq0),
            {true, wrq:set_response_code(202, Rq1), Ctx};
        RawRequestMd5 ->
            RequestMd5 = base64:decode(RawRequestMd5),
            case RequestMd5 of
                HashMd5 ->
                    Rq1 = bksw_req:with_etag(RawRequestMd5, Rq0),
                    {true, wrq:set_response_code(202, Rq1), Ctx};
                _ ->
                    %% TODO Replace with commented out code
                    Rq1 = bksw_req:with_etag(RawRequestMd5, Rq0),
                    {true, wrq:set_response_code(202, Rq1), Ctx}
                    %% TODO {true, wrq:set_response_code(406, Rq0), Ctx}
            end
    end;
write_streamed_body({Data, Next}, Rq0,
                    #context{entry_md = #db_file{data_id = DataId},
                             next_chunk_to_stream = ChunkId} = Ctx0 ) ->
    bksw_sql:add_file_chunk(DataId, ChunkId, Data),
    %% TODO Add hash accumulation here.

    Ctx = Ctx0#context{next_chunk_to_stream = ChunkId + 1},
    write_streamed_body(Next(), Rq0, Ctx).

get_header(Header, Rq) ->
    wrq:get_req_header(Header, Rq).
