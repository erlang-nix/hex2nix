%%% ---------------------------------------------------------------------------
%%%
%%% @doc
%%%  Provides the main entry point to the hex2nix tool. Pulling down the
%%%  provided hex registry (if provided) and generating the nix expressions
%%%  to the provided root
%%%
%%% ---------------------------------------------------------------------------
-module(hex2nix).

%%
%% API
%%
-export([main/1]).

-include("hex2nix.hrl").

%%
%% Types
%%
-export_type([app/0, app_version/0, deps/0,
              app_name/0]).

-type version_constraint() :: binary().

-type app_name() :: binary().
-type app_version() :: binary().

-type app() :: {app_name(), app_version()}.

-type app_detail() :: [{app_name(), version_constraint()}].

-type deps() :: #indexed_deps{}.

%%
%% Variables
%%
-define(REGISTRY_URL, "https://s3.amazonaws.com/s3.hex.pm/registry.ets.gz").


%% ============================================================================
%% Exported Functions
%% ============================================================================

-spec main([string()]) -> ok.
main([NixPkgsDir]) ->
    main([NixPkgsDir, ?REGISTRY_URL]);
main([NixPkgsDir, HexRegistry]) ->
    ok = start_dependencies(),
    {DepRoots0, AppData0} = split_data_into_versions_and_detail(get_registry(HexRegistry)),
    AppData1 = cleanup_app_data(AppData0),
    AllBuildableVersions = find_all_buildable_versions(AppData1),
    DepRoots1 = reduce_to_latest_buildable_version(AppData1, DepRoots0),
    Detail = #indexed_deps{roots=sets:from_list(DepRoots1)
                          , index=AllBuildableVersions
                          , detail=AppData1},
    Deps =
        h2n_fetcher:update_with_information_from_hex_pm(Detail
                                                       , h2n_resolver:resolve_dependencies(Detail)),
    Document = h2n_generate:nix_expression(Deps),
    OutputPath = filename:join(NixPkgsDir, "hex-packages.nix"),
    ok = filelib:ensure_dir(OutputPath),
    file:write_file(OutputPath, Document);
 main(_) ->
    h2n_util:stderr("This program expects at least one argument. The Nix "
                    "package dir and (optionally) the URL of the hex registry. "
                    "Without those at least the Nix package dir argument "
                    "it can not function.~n", []),
    erlang:halt(1).

-spec start_dependencies() -> ok.
start_dependencies() ->
    ssl:start(),
    ibrowse:start(),
    ok.

-spec get_registry(file:filename()) ->  [any()].
get_registry(HexRegistry) ->
    TempDirectory = h2n_util:temp_directory(),
    io:format("Pulling Hex Registry From ~s to ~s~n"
             , [HexRegistry, TempDirectory]),
    {ok, "200", _, {file, GzippedFileName}} =
        ibrowse:send_req(HexRegistry
                        , []
                        , get
                        , []
                        , [{save_response_to_file,
                            filename:join(TempDirectory, "registry.ets.gz")}]),
    {ok, RegistryFile} = decompress_file(GzippedFileName, TempDirectory),
    {ok, RegistryTable} = ets:file2tab(RegistryFile),
    ets:tab2list(RegistryTable).

%% ============================================================================
%% Side Effect Free Functions
%% ============================================================================

-spec split_data_into_versions_and_detail([term()]) ->
                                                 {[app()], [{app(), app_detail()}]}.
split_data_into_versions_and_detail(AppData) ->
    lists:partition(fun({Name, _}) when erlang:is_binary(Name) ->
                            true;
                       (_) ->
                            false
                    end, AppData).

-spec find_all_buildable_versions(dict:dict(app(), app_detail())) ->
                                         dict:dict(app_name(), [app_version()]).
find_all_buildable_versions(AppData) ->
    dict:fold(fun({Name, Version}, _, Acc) ->
                      dict:append(Name, Version, Acc)
                end, dict:new(), AppData).

-spec cleanup_app_data([any()]) -> dict:dict(app(), app_detail()).
cleanup_app_data(AppData) ->
    lists:foldl(fun({App, [Deps, _, BuildSystems]}, Acc) ->
                        case is_supported_build_system(BuildSystems) of
                            true ->
                                dict:store(App, simplify_deps(Deps), Acc);
                            false ->
                                Acc
                        end;
                   (X, Acc) ->
                        io:format("Discarding malformed detail: ~p~n", [X]),
                        Acc
                end, dict:new(), AppData).

-spec decompress_file(file:filename(), file:filename()) ->
                             {ok, file:filename()}.
decompress_file(GzippedFileName, TargetDirectory) ->
    ResultFile = filename:join(TargetDirectory, "registry.ets"),
    {ok, Data} = file:read_file(GzippedFileName),
    UncompressedData = zlib:gunzip(Data),
    file:write_file(ResultFile, UncompressedData, []),
    io:format("File uncompressed to ~s~n", [ResultFile]),
    {ok, ResultFile}.

-spec simplify_deps([[any()]]) ->
                           [app()].
simplify_deps(Deps) ->
    lists:map(fun simplify_dep/1, Deps).

-spec simplify_dep([app_name() | binary()]) ->
                          {app_name(), [binary()]}.
simplify_dep([Name, Constraint, _, _]) ->
    {Name, Constraint}.

-spec is_supported_build_system([binary()] | binary()) -> boolean().
is_supported_build_system(SystemList)
  when erlang:is_list(SystemList) ->
    lists:any(fun is_supported_build_system/1, SystemList);
is_supported_build_system(<<"rebar3">>) -> true;
is_supported_build_system(<<"make">>) ->  true;
is_supported_build_system(<<"rebar">>) ->  true;
is_supported_build_system(_) ->  false.

-spec reduce_to_latest_buildable_version(dict:dict(app(), app_detail()),
                                         [{app_name(), [app_version()]}]) ->
                                                [app()].
reduce_to_latest_buildable_version(AppData, AppVersions) ->
    lists:foldl(fun({Name, [Versions]}, Acc) ->
                        [HardVersion | _] = lists:sort(fun ec_semver:gte/2, Versions),
                        case dict:find({Name, HardVersion}, AppData) of
                            {ok,  _} ->
                                [{Name, HardVersion} | Acc];
                            error ->
                                Acc
                        end
              end, [], AppVersions).

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.

-spec simplify_dep_test() -> ok.
simplify_dep_test() ->
    ?assertEqual({<<"poolboy">>, <<"~> 1.5">>},
                 simplify_dep([<<"poolboy">>,<<"~> 1.5">>,false,<<"poolboy">>])),
    ?assertEqual({<<"poison">>, <<"~> 1.5">>},
                 simplify_dep([<<"poison">>,<<"~> 1.5">>,false,<<"poison">>])).

-spec filter_erlang_only_test() -> ok.
filter_erlang_only_test() ->
    ?assertEqual(false, is_supported_build_system(<<"mix">>)),
    ?assertEqual(true, is_supported_build_system([<<"rebar3">>, <<"make">>])),
    ?assertEqual(true, is_supported_build_system([<<"rebar3">>, <<"hex">>])).

-spec is_supported_build_system_test() -> ok.
is_supported_build_system_test() ->
    ?assert(is_supported_build_system([<<"rebar3">>, <<"make">>])),
    ?assertEqual(false, is_supported_build_system([<<"hex">>])),
    ?assert(is_supported_build_system([<<"rebar">>])),
    ?assertEqual(true, is_supported_build_system([<<"hex">>, <<"make">>])).


-endif.
