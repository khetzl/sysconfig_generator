#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

main([Environment, EnvsFN, TemplateFN, OutputFN]) ->
    Template = consult_template(TemplateFN),
    DynamicTerms = get_dynamic_terms(Environment, EnvsFN),    
    [SysConfig] = traverse_and_insert(Template, DynamicTerms),
    io:format("Following config was generated:~n~p~n", [SysConfig]),
    file:write_file(OutputFN, io_lib:format("~tp.~n", [SysConfig])),
    init:stop(1);
main(_) ->
    usage(),
    init:stop(1).

consult_template(TemplateFN) ->
    case file:consult(TemplateFN) of
        {error, Error} ->
            io:format("ERROR: ~p while reading template (~s)~n", [Error, TemplateFN]),
            init:stop(1);
        {ok, T} ->
            T
    end.

get_dynamic_terms(Env, EnvsFN) ->
    case file:consult(EnvsFN) of
        {error, Error} ->
            io:format("ERROR: ~p while reading environments file (~s)~n", [Error, EnvsFN]),
            init:stop(1);
        {ok, EnvironmentTerms} ->
            case proplists:get_value(list_to_atom(Env), EnvironmentTerms) of
                undefined ->
                    io:format("ERROR: Environment ~p wasn't found in file ~p~n",
                              [Env, EnvsFN]),
                    init:stop(1); %% FIXME maybe
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
    case proplists:get_value(DFile, Dynamics) of
        undefined ->
            throw({notfound_in_env, DFile});
        FileName ->
            case file:consult(FileName) of
                {error, Error} ->
                    io:format("ERROR: ~p while reading file (~s)~n", [Error, FileName]),
                    init:stop(1);
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


