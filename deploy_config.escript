#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

%% This script is very similar to the other ones, however expects, only dynamic terms
%% file with only one environment's config. This should be a local or host specific
%% environment description.

main([SysConfigPath, KeyFileName]) ->
    SysConfig = consult_sysconfig(SysConfigPath),
    DynamicTerms = get_dynamic_terms(KeyFileName),
    UpdatedSysConfig = traverse_and_insert(SysConfig, DynamicTerms),
    io:format("Following config was generated:~n~p~n", [UpdatedSysConfig]),
    file:write_file(SysConfigPath, io_lib:format("~tp.~n", [UpdatedSysConfig])),
    init:stop(1);
main(_) ->
    usage(),
    halt(1).

consult_sysconfig(Path) ->
    case file:consult(Path) of
        {error, Error} ->
            io:format("ERROR: ~p while reading sys.config (~s)~n", [Error, Path]),
            halt(1);
        {ok, [T]} ->
            T
    end.

get_dynamic_terms(EnvsFN) ->
    case file:consult(EnvsFN) of
        {error, Error} ->
            io:format("ERROR: ~p while reading local config file (~s)~n", [Error, EnvsFN]),
            halt(1);
        {ok, EnvironmentTerms} ->
            EnvironmentTerms
    end.

traverse_and_insert({'Deploy-Time', DKey}, Dynamics) ->
    case proplists:get_value(DKey, Dynamics) of
        undefined ->
            throw({notfound, DKey, Dynamics});
        Value ->
            Value
    end;
traverse_and_insert({'Deploy-Time', DKey, Default}, Dynamics) ->
    proplists:get_value(DKey, Dynamics, Default);
traverse_and_insert(List, Dynamics) when is_list(List) ->
    lists:map(fun(E) -> traverse_and_insert(E, Dynamics) end, List);
traverse_and_insert(Tuple, Dynamics) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    ReturnList = traverse_and_insert(List, Dynamics),
    list_to_tuple(ReturnList);
traverse_and_insert(Value, _Dynamics) ->
    Value.

usage() ->
    io:format("Usage: deploy_config.escript <sys_config_path> <local_key_file>~n", []).
