#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

-define(CONFIG_PREFIX, "config-").
-define(CONFIG_FILE, "sys.config").

main([EnvsFN, TemplateFN, OutputPath]) ->
    Template = chk_consult_file(TemplateFN),
    Dynamics = chk_consult_file(EnvsFN),

    SysConfigs = lists:map(fun({Environment, DynamicsTerm}) ->
                                   solve_single_config(Environment, DynamicsTerm, Template)
                           end, Dynamics),

    %% TODO: check verbose option if printout is needed:
    io:format("Following configs were generated:~n~p~n", [SysConfigs]),

    lists:foreach(fun({Environment, SysConfig}) ->
                          %% TODO: versioning
                          dump_to_file(Environment, SysConfig, OutputPath)
                  end, SysConfigs),
    init:stop(1);
main(_) ->
    usage(),
    halt(1).

chk_consult_file(FN) ->
    case file:consult(FN) of
        {error, Error} ->
            io:format("ERROR: ~p while reading file (~s)~n", [Error, FN]),
            halt(1);
        {ok, T} ->
            T
    end.

dump_to_file(Environment, SysConfig, Path) ->
    DirName = try_make_dir(Environment, Path),
    FileName = filename:join(DirName, ?CONFIG_FILE),
    file:write_file(FileName, io_lib:format("~tp.~n", [SysConfig])).

try_make_dir(Environment, Path) ->
    do_try_make_dir(Path),
    DirName = filename:join(Path, ?CONFIG_PREFIX ++ atom_to_list(Environment)),
    do_try_make_dir(DirName).

do_try_make_dir(DirName) ->
    case file:make_dir(DirName) of
        ok ->
            DirName;
        {error, eexist} ->
            DirName;
        {error, Reason} ->
            io:format("Unexpected error (~p) while creating dir:~p~n", [DirName, Reason]),
            halt(1)
    end.

solve_single_config(Environment, DynamicTerms, Template) ->
    [SysConfig] = traverse_and_insert(Template, DynamicTerms),
    {Environment, SysConfig}.


get_dynamic_terms(Env, EnvsFN) ->
    case file:consult(EnvsFN) of
        {error, Error} ->
            io:format("ERROR: ~p while reading environments file (~s)~n", [Error, EnvsFN]),
            halt(1);
        {ok, EnvironmentTerms} ->
            case proplists:get_value(list_to_atom(Env), EnvironmentTerms) of
                undefined ->
                    io:format("ERROR: Environment ~p wasn't found in file ~p~n",
                              [Env, EnvsFN]),
                    halt(1);
                Dynamics ->
                    Dynamics
            end
    end.

traverse_and_insert({'Dynamic', DKey}, Dynamics) ->
    case proplists:get_value(DKey, Dynamics) of
        undefined ->
            throw({notfound, DKey, Dynamics});
        Value ->
            Value
    end;
traverse_and_insert({'Dynamic', DKey, Default}, Dynamics) ->
    proplists:get_value(DKey, Dynamics, Default);
traverse_and_insert({'Dynamic-File', DFile, DKey}, Dynamics) ->
    %% TODO: resolve these into macros...
    case proplists:get_value(DFile, Dynamics) of
        undefined ->
            throw({notfound_in_env, DFile});
        FileName ->
            case file:consult(FileName) of
                {error, Error} ->
                    io:format("ERROR: ~p while reading file (~s)~n", [Error, FileName]),
                    halt(1);
                {ok, DFileContent} ->
                    case proplists:get_value(DKey, DFileContent) of
                        undefined ->
                            throw({not_found_in_file, FileName, DKey});
                        Value ->
                            Value
                    end
            end
    end;
traverse_and_insert(List, Dynamics) when is_list(List) ->
    lists:map(fun(E) -> traverse_and_insert(E, Dynamics) end, List);
traverse_and_insert(Tuple, Dynamics) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    ReturnList = traverse_and_insert(List, Dynamics),
    list_to_tuple(ReturnList);
traverse_and_insert(Value, _Dynamics) ->
    Value.    

usage() ->
    io:format("Usage: generate_sysconfig.escript <environment_name> <environments_file> <sys.config.template> <output_file>~n", []).


