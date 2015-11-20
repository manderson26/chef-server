%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(bksw_io).

-export([bucket_list/0,
         bucket_exists/1,
         bucket_delete/1,
         bucket_create/1]).

-export([entry_list/1,
         entry_delete/2,
         entry_exists/2]).

-export([
         disk_format_version/0,
         ensure_disk_store/0,
         upgrade_disk_format/0
         ]).

-include_lib("kernel/include/file.hrl").
-include("internal.hrl").

-define(DISK_FORMAT_VERSION, 1).
%% Use _%_ prefix since this cannot appear as a bucket name (we
%% encode bucket names so no bare %'s). Prefer this to a hidden
%% file since that reduces the chance of missing the version file
%% as part of backup/restore.
-define(FORMAT_VERSION_FILE, "_%_BOOKSHELF_DISK_FORMAT").
-define(MAGIC_NUMBER, <<16#b00c:16/integer>>).
-define(MAGIC_NUMBER_SIZE_BYTES, 2).
-define(CHECKSUM_SIZE_BYTES, 16).
-define(TOTAL_HEADER_SIZE_BYTES, ?MAGIC_NUMBER_SIZE_BYTES + ?CHECKSUM_SIZE_BYTES).

%% Matches file names without "._bkwbuf_" in the name
%%-define(DISCARD_WRITE_BUFS, "^(?:.(?<!\\._bkwbuf_))*$").
-define(WRITE_BUFS, ". [0-9][0-9][0-9]_bkwbuf$").

-spec bucket_list() -> [#bucket{}] | [].
bucket_list() ->
    ?LOG_DEBUG("reading bucket list"),
    bksw_sql:list_buckets().

-spec bucket_exists(binary()) -> boolean().
bucket_exists(Bucket) ->
    case bksw_sql:find_bucket(Bucket) of
        {ok, none} ->
            false;
        {ok, _} ->
            true
end.

-spec bucket_create(binary()) -> boolean().
bucket_create(Bucket) ->
    case bksw_sql:bucket_exists(Bucket) of
        false ->
            case bksw_sql:create_bucket(Bucket) of
                {ok, 1} ->
                    true;
                _Error ->
                    false
            end;
        true ->
            true
    end.

-spec bucket_delete(binary()) -> boolean().
bucket_delete(Bucket) ->
    case bksw_sql:delete_bucket(Bucket) of
        ok ->
            true;
        _ ->
            false
    end.

-spec entry_list(binary()) -> [#object{}] | [].
entry_list(Bucket) ->
    ?LOG_DEBUG("reading entries for bucket '~p' #~p", [Bucket]),
    bksw_sql:list_bucket(Bucket).

-spec entry_delete(binary(), binary()) -> boolean().
entry_delete(Bucket, Entry) ->
    bksw_sql:delete_file(Bucket, Entry).

-spec entry_exists(binary(), binary()) -> boolean().
entry_exists(Bucket, Path) ->
    FullPath = bksw_io_names:entry_path(Bucket, Path),
    Ans = filelib:is_regular(FullPath),
    ?LOG_DEBUG("entry_exists ~p ~p ~p", [Bucket, Path, Ans]),
    Ans.

%% @doc Return the on disk format version. If no version file is
%% found, returns `{version, 0}' which is the first shipping format.
-spec disk_format_version() -> {version, integer()}.
disk_format_version() ->
    Root = bksw_conf:disk_store(),
    VersionFile = filename:join([Root, ?FORMAT_VERSION_FILE]),
    case filelib:is_file(VersionFile) of
        false ->
            %% we assume version 0 if no version file is found.
            {version, 0};
        true ->
            {version, read_format_version(file:read_file(VersionFile))}
    end.

read_format_version({ok, Bin}) ->
    %% format version data is plain text with integer version number
    %% as first space separated token on first line.
    Line1 = hd(re:split(Bin, "\n")),
    Token1 = hd(string:tokens(binary_to_list(Line1), " ")),
    list_to_integer(Token1).

ensure_disk_store() ->
    Root = bksw_conf:disk_store(),
    ToEnsure = filename:join([Root, "placehold"]),
    case filelib:is_dir(Root) of
        true -> ?LOG_INFO("Found disk_store at ~s", [Root]);
        false -> ?LOG_INFO("Disk store dir did not exist. creating disk_store at ~s", [Root])
    end,
    ok = filelib:ensure_dir(ToEnsure),
    ok.

upgrade_disk_format() ->
    upgrade_disk_format(disk_format_version()).

upgrade_disk_format({version, ?DISK_FORMAT_VERSION}) ->
    ?LOG_INFO("Found disk format version ~p",
              [?DISK_FORMAT_VERSION]),
    ok;
upgrade_disk_format({version, 0}) ->
    ?LOG_INFO("Found disk format version 0. Starting upgrade to version ~p",
              [?DISK_FORMAT_VERSION]),
    ok = upgrade_from_v0(),
    upgrade_disk_format(disk_format_version());
upgrade_disk_format({version, X}) ->
    error({upgrade_disk_format, "unsupported upgrade", X, ?DISK_FORMAT_VERSION}).


%% write in-progress version file?
%% get list of buckets.
%% for each bucket, list of entries (flat, ignore directories).
%% within bucket, move entry to new path
%% write version file.
upgrade_from_v0() ->
    [ upgrade_bucket(B) || #bucket{name = B} <- bucket_list() ],
    write_format_version(),
    ok.

upgrade_bucket(Bucket) ->
    ?LOG_INFO("migrating bucket: ~p~n", [Bucket]),
    RawBucket = bksw_io_names:decode(Bucket),
    BucketPath = bksw_io_names:bucket_path(RawBucket),
    Entries = filelib:wildcard(bksw_util:to_string(BucketPath) ++ "/*"),
    FileEntries = [ F || F <- Entries,
                         filelib:is_dir(F) == false ],
    EntryCount = length(FileEntries),
    ?LOG_INFO("bucket ~p: found ~p entries",
               [Bucket, EntryCount]),
    init_progress_log(["bucket ", Bucket, ": "], EntryCount),
    [
     begin
         BaseName = filename:basename(F),
         %% entry_path wants the decoded name since it encodes.
         NewPath = bksw_io_names:entry_path(RawBucket,
                                            bksw_io_names:decode(BaseName)),
         ok = filelib:ensure_dir(NewPath),
         ok = file:rename(F, NewPath),
         log_progress()
     end || F <- FileEntries ].

write_format_version() ->
    Path = filename:join(bksw_conf:disk_store(), ?FORMAT_VERSION_FILE),
    ok = file:write_file(Path, io_lib:format("~B~n", [?DISK_FORMAT_VERSION])),
    ok.

init_progress_log(Prefix, Total) ->
    put(progress_log, {iolist_to_binary(Prefix), Total, 0}).

log_progress() ->
    log_progress(get(progress_log)).

log_progress(undefined) ->
    erlang:error(uninitialized);
log_progress({Prefix, Total, Current0}) ->
    Current = Current0 + 1,
    Pct = (Current * 100) div Total,
    %% emit log every 10%
    case Pct rem 10 of
        0 ->
            error_logger:info_msg("~s~p% complete (~p/~p)", [Prefix, Pct, Current, Total]);
        _ ->
            ok
    end,
    put(progress_log, {Prefix, Total, Current}),
    ok.
