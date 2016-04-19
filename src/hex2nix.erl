%%% ---------------------------------------------------------------------------%
%%
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
              app_name/0, build_systems/0]).

-type version_constraint() :: binary().

-type app_name() :: binary().
-type app_version() :: binary().

-type app() :: {app_name(), app_version()}.

-type app_detail() :: [{app_name(), version_constraint()}].

-type deps() :: #indexed_deps{}.

-type build_systems() :: mix | rebar3 | erlang_mk.
%%
%% Variables
%%
-define(REGISTRY_URL, "https://raw.githubusercontent.com/erlang-nix/hex-pm-registry-snapshots/master/registry.ets.gz").
-define(OPTIONS,
        [
         {output_path, $o, "output-path", {string, "./"}
         ,  "Output Path for the `hex-packages.nix` file"}
        , {registry_url, $r, "registry-url"
          , {string, ?REGISTRY_URL}
          , "The url of the registry to base generation on"}
        , {build,  $b, "build-packages", {boolean, false}
          , "Try to build packages with nix and filter out those which fail. "
           "Uses NIX_PATH environmental variable."}
        , {cache,  $c, "cache-result", {boolean, false}
          , "Cache the result of package download so it can be reused. "
           "This is primarily useful for testing."}
        , {filter, $f, "filter", {string, ".*"}
          , "Allows to filter packages by name using provided PCRE regex. "
           "Matching happens against strings like \"jsx_2_7_0\". Filtering "
           "happens before dependency resolution, hence to successfully import "
           "a package filter need to allow both the package and all it's "
           "dependencies."}
        , {help, $h, "help", {boolean, false}
          , "Print the help message for this command"}
        ]).


%% ============================================================================
%% Exported Functions
%% ============================================================================

-spec main([string()]) -> ok.
main(Args) ->
    case getopt:parse(?OPTIONS, Args) of
        {ok, {Opts, []}} ->
            case lists:keysearch(help, 1, Opts) of
                {value, {help, true}} ->
                    getopt:usage(?OPTIONS, "hex2nix"),
                    erlang:halt(1);
                _ ->
                    do_main(Opts)
            end;
        _ ->
            getopt:usage(?OPTIONS, "hex2nix"),
            erlang:halt(1)
    end.

-spec do_main([getopt:option()]) -> ok.
do_main(Opts) ->
    ok = start_dependencies(),
    {value, {_, HexRegistry}} = lists:keysearch(registry_url, 1, Opts),
    {value, {_, NixPkgsDir}} = lists:keysearch(output_path, 1, Opts),
    {value, {_, ShouldCache}} = lists:keysearch(cache, 1, Opts),
    {value, {_, ShouldBuild}} = lists:keysearch(build, 1, Opts),
    {value, {_, Filter}} = lists:keysearch(filter, 1, Opts),

    {DepRoots0, AppData0} = split_data_into_versions_and_detail(
                              h2n_fetcher:get_registry(ShouldCache, HexRegistry)),
    AppData1 = cleanup_app_data(Filter, AppData0),
    AllBuildableVersions = find_all_buildable_versions(AppData1),
    DepRoots1 = reduce_to_latest_buildable_version(AppData1, DepRoots0),
    Detail = #indexed_deps{roots=sets:from_list(DepRoots1)
                          , index=AllBuildableVersions
                          , detail=AppData1},
    Deps =
        ordsets:from_list(
          h2n_fetcher:update_with_information_from_hex_pm(ShouldCache
                                                         , Detail
                                                         , h2n_resolver:resolve_dependencies(Detail))),

    write_nix_expressions(Deps, ordsets:new(), NixPkgsDir),
    case ShouldBuild of
        true ->
            %% Given Nix nature it is enough to iterate over all packages and
            %% remove the ones which fail. There is no need to do graph
            %% traversal and pruning, since Nix does builds hermetically and
            %% reproducibly - it is safe to assume that if a dependency fails,
            %% the package will fail as well.
            lists:foldl(fun(Dep, Failing) ->
                                try_build_and_write_packages(Dep, Failing, Deps, NixPkgsDir)
                        end, ordsets:new(), Deps);
        false ->
            ok
    end.

-spec try_build_and_write_packages(h2n_fetcher:dep_desc(),
                                   ordsets:ordset(),
                                   ordsets:ordset(),
                                   string()) ->
                                          ordsets:ordset().
try_build_and_write_packages(#dep_desc{app = App} = Package, Failing, Deps, NixPkgsDir) ->
    io:format("Attempting to build ~p~n", [Package]),
    Failing2 = case try_build(Package) of
                   true ->
                       Failing;
                   false ->
                       io:fwrite("Removing ~p from hex-packages.nix~n", [App]),
                       ordsets:add_element(Package, Failing)
               end,
    write_nix_expressions(ordsets:subtract(Deps, Failing2),
                          Failing2,
                          NixPkgsDir),
    Failing2.

-spec try_build(h2n_fetcher:dep_desc()) -> boolean().
try_build(#dep_desc{app = App}) ->
    NixName = h2n_generate:format_name(App),
    case h2n_util:run("nix-build $NIX_PATH/nixpkgs/ -A beamPackages.~s", [NixName]) of
        {ok, _} ->
            true;
        {error, Status, Out} ->
            io:fwrite("Building of ~p failed (~b) with:~n~s~n", [App, Status, Out]),
            false
    end.

-spec write_nix_expressions(ordsets:ordset(), ordsets:ordset(), string()) -> 'ok'.
write_nix_expressions(Deps, Failing, NixPkgsDir) ->
    Document = h2n_generate:nix_expression(ordsets:to_list(Deps), ordsets:to_list(Failing)),
    OutputPath = filename:join(NixPkgsDir, "hex-packages.nix"),
    ok = filelib:ensure_dir(OutputPath),
    file:write_file(OutputPath, Document),
    io:format("Wrote 'hex-packages.nix' to ~s~n", [NixPkgsDir]).

-spec start_dependencies() -> ok.
start_dependencies() ->
    ssl:start(),
    ibrowse:start(),
    ok.

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

-spec cleanup_app_data(string(), [any()]) -> dict:dict(app(), app_detail()).
cleanup_app_data(Filter, AppData) ->
    lists:foldl(fun({App, [Deps, _, BuildSystems]}, Acc) ->
                        case (is_supported_build_system(BuildSystems) andalso
                              filter_by_name(Filter, App)) of
                            true ->
                                dict:store(App, simplify_and_remove_duplicate_deps(Deps), Acc);
                            false ->
                                Acc
                        end;
                   (X, Acc) ->
                        io:format("Discarding malformed detail: ~p~n", [X]),
                        Acc
                end, dict:new(), AppData).

-spec filter_by_name(string(), #dep_desc{}) -> boolean().
filter_by_name(Filter, App) ->
    NixName = h2n_generate:format_name(App),
    case re:run(NixName, Filter) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.

-spec simplify_and_remove_duplicate_deps([[any()]]) ->
                           [app()].
simplify_and_remove_duplicate_deps(Deps) ->
    lists:foldl(fun (El0, Acc) ->
                        El1 = {Name, _} = simplify_dep(El0),
                        case lists:keymember(Name, 1, Acc) of
                            true ->
                                Acc;
                            false ->
                                [ El1 | Acc ]
                        end
                end, [], Deps).


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
is_supported_build_system(<<"mix">>) ->  true;
is_supported_build_system(_) ->  false.


-spec reduce_to_latest_buildable_version(dict:dict(app(), app_detail()),
                                         [{app_name(), [app_version()]}]) ->
                                                [app()].
reduce_to_latest_buildable_version(AppData, AppVersions) ->
    lists:foldl(
      fun({Name, [Versions]}, Acc) ->
              SortedVersions = lists:sort(fun ec_semver:gte/2, Versions),
              case find_latest_available_version(Name, SortedVersions, AppData) of
                  {ok,  HardVersion} ->
                      [{Name, HardVersion} | Acc];
                  error ->
                      Acc
              end
      end, [], AppVersions).

-spec find_latest_available_version(app_name(),
                                    [app_version()],
                                    dict:dict(app(), app_detail())) ->
                                           {ok, app_version()} | error.
find_latest_available_version(_, [], _) ->
    error;
find_latest_available_version(Name, [HardVersion | Versions], AppData) ->
    case dict:find({Name, HardVersion}, AppData) of
        {ok,  _} ->
            {ok, HardVersion};
        error ->
            find_latest_available_version(Name, Versions, AppData)
    end.

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
    ?assertEqual(true, is_supported_build_system(<<"mix">>)),
    ?assertEqual(true, is_supported_build_system([<<"rebar3">>, <<"make">>])),
    ?assertEqual(true, is_supported_build_system([<<"rebar3">>, <<"hex">>])).

-spec is_supported_build_system_test() -> ok.
is_supported_build_system_test() ->
    ?assert(is_supported_build_system([<<"rebar3">>, <<"make">>])),
    ?assertEqual(false, is_supported_build_system([<<"hex">>])),
    ?assert(is_supported_build_system([<<"rebar">>])),
    ?assertEqual(true, is_supported_build_system([<<"hex">>, <<"make">>])).


-endif.
