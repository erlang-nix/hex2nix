%%% ---------------------------------------------------------------------------
%%% @doc
%%%
%%% This module resolves the dependencies. It is *not* a full
%%% constraint solver. It does *not* make sure that everything meets
%%% the dep constraints. What it does do is follow very, very closely
%%% the logic for depednency resolution in rebar3. You should note
%%% that this logic is not the same as that in Mix. So when and if we
%%% add Mix support we are going to need to do something different.
%%% ---------------------------------------------------------------------------
-module(h2n_resolver).

%%
%% API
%%
-export([resolve_dependencies/1]).

%%
%% Types
%%
-export_type([app_dep/0]).

-type app_dep() :: {hex2nix:app(), [hex2nix:app()]}.
-type constraint() :: '>' | '~>' | '<=' | '>=' | '='.

-include("hex2nix.hrl").


%% ============================================================================
%% Exported Functions
%% ============================================================================
-spec resolve_dependencies(hex2nix:deps()) ->
                                   [{hex2nix:app(), [hex2nix:app()]}].
resolve_dependencies(Deps = #indexed_deps{roots = Roots}) ->
    {AppDescs, Resolved} = expand_dependencies(Deps),
    NeedsResolution = sets:subtract(Resolved, Roots),
    case sets:size(NeedsResolution) of
        0 ->
            AppDescs;
        _ ->
            AppDescs ++ resolve_dependencies(Deps#indexed_deps{roots=NeedsResolution})
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================
-spec expand_dependencies(hex2nix:deps()) ->
                                 {[{hex2nix:app(), [hex2nix:app()]}],
                                  sets:set(hex2nix:app())}.
expand_dependencies(Deps = #indexed_deps{roots = Roots}) ->
    sets:fold(fun(App = {Name, Vsn}, Acc) ->
                      try expand_dependencies(Deps, App, Acc) of
                          Result -> Result
                      catch
                          throw:{missing_dependency
                                , MatchType
                                , {DepName, DepVsn}} ->
                              io:format("Unable to resolve dependency (~s ~s ~s) "
                                        "for ~s-~s~n"
                                       ,  [DepName, MatchType
                                          , DepVsn, Name, Vsn]),
                              Acc
                      end
              end, {[], sets:new()}, Roots).

-spec expand_dependencies(hex2nix:deps()
                         , hex2nix:app()
                         , {[{hex2nix:app(), [hex2nix:app()]}]
                           , sets:set(hex2nix:app())}) ->
                                 {[{hex2nix:app(), [hex2nix:app()]}]
                                 , sets:set(hex2nix:app())}.
expand_dependencies(Deps = #indexed_deps{detail = Detail}
                   , App
                   , {Acc, AllDeps}) ->
    case dict:find(App, Detail) of
        {ok, AppDeps} ->
            DepsList = update_deps_list(Deps, AppDeps),
            {[{App, DepsList} | Acc],
             sets:union(AllDeps
                       , resolve_deps(Deps
                                     , {[App | DepsList], AllDeps}))};
        error ->
            erlang:throw({missing_dependency, '=', App})
    end.


-spec resolve_deps(hex2nix:deps(), {[hex2nix:app()], set:set(hex2nix:app())}) ->
                          set:set(hex2nix:app()).
resolve_deps(_, {[], Seen}) ->
    Seen;
resolve_deps(Deps = #indexed_deps{detail = Detail},
             {AppList, Seen0}) ->
    resolve_deps(Deps,
                 lists:foldl(fun(App, {Acc, Seen1}) ->
                                     case dict:find(App, Detail) of
                                         {ok, AppDeps} ->
                                             DepsList =
                                                 filter_seen(update_deps_list(Deps
                                                                             , AppDeps)
                                                            , Seen1),
                                             {[DepsList | Acc], sets:add_element(App, Seen1)};
                                         error ->
                                             erlang:throw({missing_dependency, '=', App})
                                     end
                             end, {[], Seen0}, lists:flatten(AppList))).

-spec filter_seen([hex2nix:app()], set:set(hex2nix:app())) -> [hex2nix:app()].
filter_seen(AppList, Acc) ->
    lists:filter(fun(App) ->
                         not sets:is_element(App, Acc)
                 end, AppList).


-spec find_highest_matching_0(hex2nix:deps(), binary(), constraint(), binary(),
                            [hex2nix:app()]) -> [hex2nix:app()].
find_highest_matching_0(Deps, Dep, Constraint, Vsn, DepsListAcc) ->
    {ok, HighestDepVsn} =
        find_highest_matching_1(Deps, Dep, Constraint,
                                h2n_util:trim(Vsn)),
    [{Dep, HighestDepVsn} | DepsListAcc].

-spec update_deps_list(hex2nix:deps(), [hex2nix:app()]) -> [hex2nix:app()].
update_deps_list(Deps, AppDeps) ->
    lists:foldl(fun({Dep, DepVsn}, DepsListAcc) ->
                        case DepVsn of
                            <<"=", Vsn/binary>> ->
                                find_highest_matching_0(Deps, Dep, '=',
                                                        Vsn, DepsListAcc);
                            <<"<=", Vsn/binary>> ->
                                find_highest_matching_0(Deps, Dep, '<=',
                                                        Vsn, DepsListAcc);
                            <<">=", Vsn/binary>> ->
                                find_highest_matching_0(Deps, Dep, '>=',
                                                        Vsn, DepsListAcc);
                            <<">", Vsn/binary>> ->
                                find_highest_matching_0(Deps, Dep, '>',
                                                        Vsn, DepsListAcc);
                            <<"~>", Vsn/binary>> ->
                                find_highest_matching_0(Deps, Dep, '~>',
                                                        Vsn, DepsListAcc);
                            nil ->
                                find_highest_matching_0(Deps, Dep, '>=',
                                                        <<"0.0.0">>, DepsListAcc);
                            Vsn ->
                                [{Dep, h2n_util:trim(Vsn)} | DepsListAcc]
                        end
                end, [], AppDeps).

%% Hex supports use of ~> to specify the version required for a dependency.
%% Since rebar3 requires exact versions to choose from we find the highest
%% available version of the dep that passes the constraint.

%% `~>` will never include pre-release versions of its upper bound.
%% It can also be used to set an upper bound on only the major
%% version part. See the table below for `~>` requirements and
%% their corresponding translation.
%% `~>` | Translation
%% :------------- | :---------------------
%% `~> 2.0.0` | `>= 2.0.0 and < 2.1.0`
%% `~> 2.1.2` | `>= 2.1.2 and < 2.2.0`
%% `~> 2.1.3-dev` | `>= 2.1.3-dev and < 2.2.0`
%% `~> 2.0` | `>= 2.0.0 and < 3.0.0`
%% `~> 2.1` | `>= 2.1.0 and < 3.0.0`
-spec find_highest_matching_1(hex2nix:deps(), binary(), '>' | '~>' | '<=' | '>=' | '=', binary()) ->
                                   {ok, binary()} | none.
find_highest_matching_1(#indexed_deps{index = Index}, Name, MatchType, Constraint) ->
    case dict:find(Name, Index) of
        {ok, [Vsn]} when erlang:is_binary(Vsn) ->
            handle_single_vsn(Name, Vsn, MatchType, Constraint);
        {ok, [HeadVsn | VsnTail]} ->
            {ok, handle_vsns(Constraint, MatchType, HeadVsn, VsnTail)};
        error ->
            io:format("!!! HERE ~p: ~p~n", [?LINE, Name]),
            erlang:throw({missing_dependency, MatchType, {Name, Constraint}})
    end.

-spec matches('>' | '~>' | '<=' | '>=' | '=', binary(), binary(), binary()) -> boolean().
matches('~>', Constraint, Version, Highest) ->
    ec_semver:pes(Version, Constraint) andalso
        ec_semver:gt(Version, Highest);
matches('>', _Constraint, Version, Highest) ->
    ec_semver:gt(Version, Highest);
matches('<=', _Constraint, Version, Highest) ->
    ec_semver:lte(Version, Highest);
matches('>=', _Constraint, Version, Highest) ->
    ec_semver:gte(Version, Highest);
matches('=', _Constraint, Version, Highest) ->
    ec_semver:eql(Version, Highest).

-spec handle_vsns(binary(), '>' | '~>' | '<=' | '>=' | '=' , binary(), [binary()]) -> binary().
handle_vsns(Constraint, MatchType, HeadVsn, VsnTail) ->
    lists:foldl(fun(Version, Highest) ->
                        case  matches(MatchType
                                     , Constraint
                                     , Version
                                     , Highest) of
                            true ->
                                Version;
                            false ->
                                Highest
                        end
                end, HeadVsn, VsnTail).

-spec matches_single('>' | '~>' | '<=' | '>=' | '=', binary(), binary()) -> boolean().
matches_single('~>', Vsn, Constraint) ->
    ec_semver:pes(Vsn, Constraint);
matches_single('>', Vsn, Constraint) ->
    ec_semver:gt(Vsn, Constraint);
matches_single('<=', Vsn, Constraint) ->
    ec_semver:lte(Vsn, Constraint);
matches_single('>=', Vsn, Constraint) ->
    ec_semver:gte(Vsn, Constraint);
matches_single('=', Vsn, Constraint) ->
    ec_semver:eql(Vsn, Constraint).

-spec handle_single_vsn(binary(), binary(), '>' | '~>', binary()) ->
                               {ok, binary()} | none.
handle_single_vsn(Name, Vsn, MatchType, Constraint) ->
    case matches_single(MatchType, Vsn, Constraint) of
        true ->
            {ok, Vsn};
        false ->
            erlang:throw({missing_dependency, MatchType, {Name, Constraint}})
    end.
