%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%

%% Placeholder module for darklaunch redis functions.
%%
-module(mover_org_darklaunch).

-export([disable_org/1,
         enable_org/1,
         init_org_to_couch/2,
         org_to_couch/2,
         org_to_sql/2,
         enable_solr4/1,
         enable_both_solrs/1,
         enable_solr1/1]).

disable_org(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HSET", OrgKey, "503_mode", "true"]).

enable_org(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HSET", OrgKey, "503_mode", "false"]).

org_to_couch(OrgName, Components) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    PropKVs = lists:foldl(fun(X, Accum) -> ["couchdb_" ++ atom_to_list(X), "true" | Accum] end, [], Components),
    send_eredis_q(["HMSET", OrgKey] ++ PropKVs).

init_org_to_couch(OrgName, Components) ->
    %% Use HSETNX so that couchdb_* flags are set only if it does not already exist
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    [send_eredis_q(["HSETNX", OrgKey, "couchdb_" ++ atom_to_list(X), "true"]) || X <- Components].

org_to_sql(OrgName, Components) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    PropKVs = lists:foldl(fun(X, Accum) -> ["couchdb_" ++ atom_to_list(X), "false" | Accum] end, [], Components),
    send_eredis_q(["HMSET", OrgKey] ++ PropKVs).

%% Enables solr4 and disables the paired sending to solr1.4 and solr4.
enable_solr4(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HMSET", OrgKey, "solr4", "true", "query_aux_solr", "false", "rabbit_aux_vhost", "false"]).

%% Enables the paired sending to solr1.4 and solr4 and disables sending only to sol4.
enable_both_solrs(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HMSET", OrgKey, "solr4", "false", "query_aux_solr", "true", "rabbit_aux_vhost", "true"]).

enable_solr1(OrgName) ->
    OrgKey = iolist_to_binary(["dl_org_", OrgName]),
    send_eredis_q(["HMSET", OrgKey, "solr4", "false", "query_aux_solr", "false", "rabbit_aux_vhost", "false"]).

send_eredis_q(Command) ->
    %% if we're configured for- dry_run mode, don't send the commands to redis
    send_eredis_q(envy:get(mover, dry_run, boolean), Command).

send_eredis_q(true, _) ->
    ok;
send_eredis_q(false, Command) ->
    case eredis:q(mover_eredis_client, Command) of
        {ok, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

