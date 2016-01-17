%%% ---------------------------------------------------------------------------
%%% @doc
%%%
%%% This module fetches information from `Hex.pm` using its restful
%%% API. *Note* The `Hex.pm` api is rate limited. Roughly 100 requests
%%% in 60 seconds is The maximum allowed. We add a limit here of 1
%%% request every second to make sure we don't go over that. If any
%%% call Fails here, the entire process fails.
%%% ---------------------------------------------------------------------------
-module(h2n_fetcher).

%%
%% API
%%
-export([update_with_information_from_hex_pm/3]).

%%
%% Imports
%%
-include("hex2nix.hrl").

%%
%% Types
%%
-export_type([license/0
             , link/0
             , position/0
             , dep_desc/0
             , sha/0]).

-type license() :: binary().
-type description() :: binary() | none.
-type link() :: binary() | no_source.
-type position() :: root | historical.
-type dep_desc() :: #dep_desc{}.
-type sha() :: binary(). %% A sha 256 hash

%%
%% Definitions
%%

-define(CACHE_FILE, ".hex2nix.cache").
-define(DEFAULT_CDN, "https://s3.amazonaws.com/s3.hex.pm/tarballs").


%% ============================================================================
%% Exported Functions
%% ============================================================================
-spec update_with_information_from_hex_pm(boolean()
                                         , hex2nix:deps()
                                         , [h2n_resolver:app_dep()]) ->
                                         [dep_desc()].
update_with_information_from_hex_pm(true, AllApps, Deps) ->
    update_with_information_from_hex_pm_with_cache(AllApps, Deps);
update_with_information_from_hex_pm(false, AllApps, Deps) ->
    update_with_information_from_hex_pm_direct(AllApps, Deps).


%% ============================================================================
%% Internal Functions
%% ============================================================================

-spec update_with_information_from_hex_pm_with_cache(hex2nix:deps()
                                                    , [h2n_resolver:app_dep()]) ->
                                                            [dep_desc()].
update_with_information_from_hex_pm_with_cache(AllApps, Deps) ->
    case file:consult(?CACHE_FILE) of
        {ok, [NewDeps]} ->
            io:format("Loading deps from cache: ~s~n", [?CACHE_FILE]),
            NewDeps;
        _ ->
            NewDeps = update_with_information_from_hex_pm_direct(AllApps, Deps),
            ec_file:write_term(?CACHE_FILE, NewDeps),
            NewDeps
    end.

-spec update_with_information_from_hex_pm_direct(hex2nix:deps(),
                                                 [h2n_resolver:app_dep()]) ->
                                                  [dep_desc()].
update_with_information_from_hex_pm_direct(AllApps, Deps) ->
    lists:map(fun (App) ->
                      decorate_app(AllApps, App)
              end, Deps).

-spec decorate_app(hex2nix:deps(), {hex2nix:app(), [hex2nix:app()]}) ->
                          dep_desc().
decorate_app(#indexed_deps{roots=Roots},
             {App={AppName, AppVsn}, Deps}) ->
    IsRoot = case sets:is_element(App, Roots) of
                 true ->
                     root;
                 false ->
                     historical
             end,
    {Description, Licenses, Link} =
        get_metadata(AppName, get_app_detail_from_hex_pm(AppName)),
    {Sha, HasNativeCode, BuildPlugins} = get_deep_meta_for_package(AppName, AppVsn),
    #dep_desc{app = App
             , description = Description
             , position = IsRoot
             , licenses = Licenses
             , homepage = Link
             , sha = Sha
             , build_plugins = BuildPlugins
             , has_native_code = HasNativeCode
             , deps = Deps}.

-spec get_app_detail_from_hex_pm(binary()) -> jsx:json_term().
get_app_detail_from_hex_pm(AppName) ->
    Thunk =
        fun() ->
                Url = h2n_util:iolist_to_list(["https://hex.pm/api/packages/"
                                              , AppName]),
                ibrowse:send_req(Url
                                , [{"Accept", "application/json"}]
                                , get
                                , []
                                , [])
        end,
    {ok, "200", _, Body} =
        take_at_least_one_second(Thunk),
    jsx:decode(erlang:iolist_to_binary(Body)).

-spec get_deep_meta_for_package(hex2nix:app_name(), hex2nix:app_version()) ->
                                 {sha(), boolean(), [hex2nix:app_name()]}.
get_deep_meta_for_package(AppName, AppVsn) ->
    TempDirectory = h2n_util:temp_directory(),
    Package = binary_to_list(<<AppName/binary, "-", AppVsn/binary, ".tar">>),
    TargetPath = filename:join(TempDirectory, Package),
    Url = h2n_util:iolist_to_list([?DEFAULT_CDN, "/", Package]),
    io:format("Pulling From ~s to ~s~n"
             , [Url, TargetPath]),
    {ok, "200", _, {file, DownloadedPath}} =
        ibrowse:send_req(Url
                        , []
                        , get
                        , []
                        , [{save_response_to_file,
                            TargetPath}]),
    [Sha | _]  = string:tokens(h2n_util:cmd("sha256sum \"~s\"", [DownloadedPath]),
                               " "),
    io:format("Got Sha ~s for ~s~n", [Sha, Package]),
    {HasNativeCode, BuildPlugins} = has_native_code_and_plugins(TempDirectory, TargetPath),
    {erlang:list_to_binary(Sha), HasNativeCode, BuildPlugins}.

-spec has_native_code_and_plugins(file:filename(), file:filename()) -> {boolean(), [hex2nix:app_name()]}.
has_native_code_and_plugins(TempDirectory, TargetPath) ->
    ok = erl_tar:extract(TargetPath, [{cwd, TempDirectory}]),
    ContentsPath = filename:join(TempDirectory, "contents.tar.gz"),
    {ok, DirListing} = erl_tar:table(ContentsPath, [compressed]) ,
    HasCSrc = lists:any(fun(Path) ->
                                lists:prefix("c_src/", Path)
                        end, DirListing),
    ok = erl_tar:extract(ContentsPath, [{cwd, TempDirectory}
                                       , {files, "rebar.config"},
                                        compressed]),
    {HasPortSpec, BuildPlugins} = analyze_rebar_config(TempDirectory),
    {HasCSrc orelse HasPortSpec, BuildPlugins}.

-spec analyze_rebar_config(file:filename()) -> {boolean(), [hex2nix:app_name()]}.
analyze_rebar_config(TempDirectory) ->
    case file:consult(filename:join(TempDirectory, "rebar.config")) of
        {ok, [Options]} ->
            BuildPlugins = lists:map(fun simplify_plugin/1,
                                     proplists:get_value(plugins, Options, [])),
            {lists:keymember("port_spec", 1, Options), BuildPlugins};
        _ ->
            {false, []}
    end.

-spec simplify_plugin(atom() | {atom(), any(), any()}) -> binary().
simplify_plugin({Name, _Vsn, _Vcs}) when is_atom(Name) ->
    atom_to_binary(Name, latin1);
simplify_plugin(Name) when is_atom(Name) ->
    atom_to_binary(Name, latin1).

-spec get_metadata(hex2nix:app_name(), {[{binary(), binary()}]} | any()) ->
                          {description(), [license()], link()}.
get_metadata(AppName, Object)
  when erlang:is_list(Object) ->
    {ok, Meta} = h2n_util:json_get_assoc_list(<<"meta">>, Object),
    Description = get_description(Meta),
    Licenses = parse_license(AppName
                            , h2n_util:json_get_list(<<"licenses">>, Meta)),
    Link = parse_links(AppName
                      , h2n_util:json_get_assoc_list(<<"links">>, Meta)),
    {Description, Licenses, Link};
get_metadata(AppName, _) ->
    io:format("Invalid metadata for ~s~n", [AppName]),
    erlang:halt(1).

-spec get_description([{binary(), jsx:json_term()}]) -> binary() | none.
get_description(Meta) ->
    case lists:keysearch(<<"description">>, 1, Meta) of
        {value, {_, Description}} when erlang:is_binary(Description) ->
            Description;
        _ ->
            none
    end.


-spec parse_links(binary(),
                  {ok, [{binary(), jsx:json_term()}] | jsx:json_term()} | false) ->
                         link().
parse_links(Name, {ok, Links})
  when erlang:is_list(Links) ->
    get_website_link(Name, lists:map(fun({Key, Link}) ->
                                                {h2n_util:binary_to_lower(Key)
                                                , Link}
                                     end,
                                     Links));
parse_links(Name, false) ->
    io:format("No homepage link for ~s~n", [Name]),
    no_source.



-spec get_website_link(hex2nix:app_name(), [{binary(), jsx:json_term()}]) -> link().
get_website_link(Name, Links)
  when erlang:is_list(Links) ->
    case lists:keysearch(<<"website">>, 1, Links) of
        {value, {_, Link}} when erlang:is_binary(Link) ->
            Link;
        _ ->
            get_source_link(Name, Links)
    end.

-spec get_source_link(hex2nix:app_name(), [{binary(), jsx:json_term()}]) -> link().
get_source_link(Name, Links) ->
    case lists:keysearch(<<"source">>, 1, Links) of
        {value, {_, Link}} when erlang:is_binary(Link) ->
            Link;
        _ ->
            get_github_link(Name, Links)
    end.

-spec get_github_link(hex2nix:app_name(), [{binary(), binary()}]) -> link().
get_github_link(Name, Links) ->
    case lists:keysearch(<<"github">>, 1, Links) of
        {value, {_, Link}} when erlang:is_binary(Link) ->
            Link;
        _ ->
            io:format("No website link found for ~p~n", [Name]),
            no_source
    end.

%% This defaults to the generic 'free' license. I don't like that, but
%% the hex api allows free form license names that means that we can't
%% anticipate all the possible types of licenses. Hopefully, we cover
%% the majority here.
-spec parse_license(hex2nix:app_name()
                   , {ok, [jsx:json_term()]| jsx:json_term()} | false) ->
                           [license()].
parse_license(Name, {ok, Licenses}) when erlang:is_list(Licenses) ->
    lists:map(fun(License) ->
                      parse_license0(Name, License)
              end, Licenses);
parse_license(Name, _) ->
    io:format("Unable to parse license for ~s. Using "
              "'Unspecified free software license'~n", [Name]),
    [<<"free">>].

-spec parse_license0(hex2nix:app_name(),
                     jsx:json_term()) -> license().
parse_license0(Name, License) when erlang:is_binary(License) ->
    parse_license1(Name,
                   erlang:list_to_binary(
                     string:to_lower(erlang:binary_to_list(License))));
parse_license0(Name, _) ->
    io:format("Unable to parse license for ~s. Using "
              "'Unspecified free software license'~n", [Name]),
    <<"free">>.


-spec parse_license1(hex2nix:app_name(), binary()) -> license().
parse_license1(_Name, <<"zpl-2.1">>) -> <<"zpt21">>;
parse_license1(_Name, <<"zpl 2.1">>) -> <<"zpt21">>;
parse_license1(_Name, <<"zpl-2.0">>) -> <<"zpt20">>;
parse_license1(_Name, <<"zpl 2.0">>) -> <<"zpt20">>;
parse_license1(_Name, <<"zlib">>) -> <<"zlib">>;
parse_license1(_Name, <<"w3c">>) -> <<"w3c">>;
parse_license1(_Name, <<"vsl-1.0">>) -> <<"vsl10">>;
parse_license1(_Name, <<"vsl 1.0">>) -> <<"vsl10">>;
parse_license1(_Name, <<"vim">>) -> <<"vim">>;
parse_license1(_Name, <<"tcl">>) -> <<"tcltk">>;
parse_license1(_Name, <<"sleepycat">>) -> <<"sleepycat">>;
parse_license1(_Name, <<"sgi-b-2.0">>) -> <<"sgi-b-20">>;
parse_license1(_Name, <<"sgi b 2.0">>) -> <<"sgi-b-20">>;
parse_license1(_Name, <<"ruby">>) -> <<"ruby">>;
parse_license1(_Name, <<"qwt">>) -> <<"qwt">>;
parse_license1(_Name, <<"qpl-1.0">>) -> <<"qpll">>;
parse_license1(_Name, <<"qpl 1.0">>) -> <<"qpll">>;
parse_license1(_Name, <<"public domain">>) -> <<"publicDomain">>;
parse_license1(_Name, <<"python-2.0">>) -> <<"psfl">>;
parse_license1(_Name, <<"postgresql">>) -> <<"postgresql">>;
parse_license1(_Name, <<"php-3.01">>) -> <<"php301">>;
parse_license1(_Name, <<"php 3.01">>) -> <<"php301">>;
parse_license1(_Name, <<"oppenssl-2.0">>) -> <<"openssl">>;
parse_license1(_Name, <<"oppenssl 2.0">>) -> <<"openssl">>;
parse_license1(_Name, <<"oldap-2.0">>) -> <<"openldap">>;
parse_license1(_Name, <<"oldap 2.0">>) -> <<"openldap">>;
parse_license1(_Name, <<"ofl-1.1">>) -> <<"ofl">>;
parse_license1(_Name, <<"ofl 1.1">>) -> <<"ofl">>;
parse_license1(_Name, <<"ncsa">>) -> <<"ncsa">>;
parse_license1(_Name, <<"msrla">>) -> <<"msrla">>;
parse_license1(_Name, <<"mpl 2.0">>) -> <<"mpl20">>;
parse_license1(_Name, <<"mpl-2.0">>) -> <<"mpl20">>;
parse_license1(_Name, <<"mpl-1.1">>) -> <<"mpl11">>;
parse_license1(_Name, <<"mpl 1.1">>) -> <<"mpl11">>;
parse_license1(_Name, <<"mpl-1.0">>) -> <<"mpl10">>;
parse_license1(_Name, <<"mpl 1.0">>) -> <<"mpl10">>;
parse_license1(_Name, <<"lpl-1.02">>) -> <<"lpl-102">>;
parse_license1(_Name, <<"lpl 1.02">>) -> <<"lpl-102">>;
parse_license1(_Name, <<"llppl-1.3c">>) -> <<"lppl13c">>;
parse_license1(_Name, <<"llppl 1.3c">>) -> <<"lppl13c">>;
parse_license1(_Name, <<"llppl-1.2">>) -> <<"lppl12">>;
parse_license1(_Name, <<"llppl 1.2">>) -> <<"lppl12">>;
parse_license1(_Name, <<"llgpl-2.1">>) -> <<"llpgl21">>;
parse_license1(_Name, <<"llgpl 2.1">>) -> <<"llpgl21">>;
parse_license1(_Name, <<"libtiff">>) -> <<"libtiff">>;
parse_license1(_Name, <<"libpng">>) -> <<"libpng">>;
parse_license1(_Name, <<"lgpl-3.0+">>) -> <<"lpgl3Plus">>;
parse_license1(_Name, <<"lgpl 3.0+">>) -> <<"lpgl3Plus">>;
parse_license1(_Name, <<"lgpl-3.0">>) -> <<"lpgl3">>;
parse_license1(_Name, <<"lgpl 3.0">>) -> <<"lpgl3">>;
parse_license1(_Name, <<"lgpl-2.1+">>) -> <<"lpgl21Plus">>;
parse_license1(_Name, <<"lgpl 2.1+">>) -> <<"lpgl21Plus">>;
parse_license1(_Name, <<"lgpl-2.1">>) -> <<"lpgl1">>;
parse_license1(_Name, <<"lgpl 2.1">>) -> <<"lpgl1">>;
parse_license1(_Name, <<"lgpl-2.0+">>) -> <<"lpgl2Plus">>;
parse_license1(_Name, <<"lgpl 2.0+">>) -> <<"lpgl2Plus">>;
parse_license1(_Name, <<"lgpl-2.0">>) -> <<"lpgl2">>;
parse_license1(_Name, <<"lgpl 2.0">>) -> <<"lpgl2">>;
parse_license1(_Name, <<"ipl-1.0">>) -> <<"ipl10">>;
parse_license1(_Name, <<"ipl 1.0">>) -> <<"ipl10">>;
parse_license1(_Name, <<"IPA">>) -> <<"ipa">>;
parse_license1(_Name, <<"IJG">>) -> <<"ijg">>;
parse_license1(_Name, <<"iasl">>) -> <<"iasl">>;
parse_license1(_Name, <<"gpl-3.0+">>) -> <<"gpl3Plus">>;
parse_license1(_Name, <<"gpl 3.0+">>) -> <<"gpl3Plus">>;
parse_license1(_Name, <<"gpl-3.0">>) -> <<"gpl3">>;
parse_license1(_Name, <<"gpl 3.0">>) -> <<"gpl3">>;
parse_license1(_Name, <<"gpl-2.0+">>) -> <<"gpl2Plus">>;
parse_license1(_Name, <<"gpl 2.0+">>) -> <<"gpl2Plus">>;
parse_license1(_Name, <<"gpl-2.0">>) -> <<"gpl2">>;
parse_license1(_Name, <<"gpl 2.0">>) -> <<"gpl2">>;
parse_license1(_Name, <<"gfdl-1.3">>) -> <<"fdl13">>;
parse_license1(_Name, <<"gfdl 1.3">>) -> <<"fdl13">>;
parse_license1(_Name, <<"gfdl-1.2">>) -> <<"fdl12">>;
parse_license1(_Name, <<"gfdl 1.2">>) -> <<"fdl12">>;
parse_license1(_Name, <<"epl-1.0">>) -> <<"epl10">>;
parse_license1(_Name, <<"epl 1.0">>) -> <<"epl10">>;
parse_license1(_Name, <<"efl-2.0">>) -> <<"efl20">>;
parse_license1(_Name, <<"efl 2.0">>) -> <<"efl20">>;
parse_license1(_Name, <<"efl-1.0">>) -> <<"efl10">>;
parse_license1(_Name, <<"efl 1.0">>) -> <<"efl10">>;
parse_license1(_Name, <<"cpl-1.0">>) -> <<"cpl10">>;
parse_license1(_Name, <<"cpl 1.0">>) -> <<"cpl10">>;
parse_license1(_Name, <<"cecill-c">>) -> <<"cecill-c">>;
parse_license1(_Name, <<"cecill c">>) -> <<"cecill-c">>;
parse_license1(_Name, <<"cecill-2.0">>) -> <<"cecill20">>;
parse_license1(_Name, <<"cecill 2.0">>) -> <<"cecill20">>;
parse_license1(_Name, <<"cddl">>) -> <<"cddl">>;
parse_license1(_Name, <<"cddl-1.0">>) -> <<"cddl">>;
parse_license1(_Name, <<"cddl 1.0">>) -> <<"cddl">>;
parse_license1(_Name, <<"cc-by-sa-4.0">>) -> <<"cc-by-sa-40">>;
parse_license1(_Name, <<"cc by sa 4.0">>) -> <<"cc-by--sa 40">>;
parse_license1(_Name, <<"cc-by-4.0">>) -> <<"cc-by-40">>;
parse_license1(_Name, <<"cc by 4.0">>) -> <<"cc-by-40">>;
parse_license1(_Name, <<"cc-by-sa-3.0">>) -> <<"cc-by-sa-30">>;
parse_license1(_Name, <<"cc-by-3.0">>) -> <<"cc-by-30">>;
parse_license1(_Name, <<"cc by 3.0">>) -> <<"cc-by-30">>;
parse_license1(_Name, <<"cc-by-sa-2.5">>) -> <<"cc-by-sa-25">>;
parse_license1(_Name, <<"cc by sa 2.5">>) -> <<"cc-by-sa-25">>;
parse_license1(_Name, <<"cc0-1.0">>) -> <<"cc0">>;
parse_license1(_Name, <<"cc0 1.0">>) -> <<"cc0">>;
parse_license1(_Name, <<"bsd-3">>) -> <<"bsd3">>;
parse_license1(_Name, <<"bsd 3">>) -> <<"bsd3">>;
parse_license1(_Name, <<"bsd-2">>) -> <<"bsd2">>;
parse_license1(_Name, <<"bsd 2">>) -> <<"bsd2">>;
parse_license1(_Name, <<"bsl-1.0">>) -> <<"boost">>;
parse_license1(_Name, <<"bsl 1.0">>) -> <<"boost">>;
parse_license1(_Name, <<"apache-2.0">>) -> <<"asl20">>;
parse_license1(_Name, <<"apache 2.0">>) -> <<"asl20">>;
parse_license1(_Name, <<"artistic-1.0">>) -> <<"artistic1">>;
parse_license1(_Name, <<"artistic 1.0">>) -> <<"artistic1">>;
parse_license1(_Name, <<"artistic">>) -> <<"artistic1">>;
parse_license1(_Name, <<"apsl-2.0">>) -> <<"apsl20">>;
parse_license1(_Name, <<"apsl 2.0">>) -> <<"apsl20">>;
parse_license1(_Name, <<"agpl-3.0">>) -> <<"agpl3">>;
parse_license1(_Name, <<"agpl 3.0">>) -> <<"agpl3">>;
parse_license1(_Name, <<"afl-2.1">>) -> <<"afl21">>;
parse_license1(_Name, <<"afl 2.1">>) -> <<"afl21">>;
parse_license1(_Name, <<"apache 2">>) -> <<"apsl20">>;
parse_license1(_Name, <<"unlicense">>) -> <<"unlicense">>;
parse_license1(_Name, <<"mozilla public license 1.1">>) -> <<"mpl11">>;
parse_license1(_Name, <<"isc">>) -> <<"isc">>;
parse_license1(_Name, <<"the mit License (mit)">>) -> <<"mit">>;
parse_license1(_Name, <<"mit License">>) -> <<"mit">>;
parse_license1(_Name, <<"gpl (for code)">>) -> <<"gpl3">>;
parse_license1(_Name, <<"bsd (for code)">>) -> <<"bsd3">>;
parse_license1(_Name, <<"mit">>) -> <<"mit">>;
parse_license1(_Name, <<"bsd">>) -> <<"bsd3">>;
parse_license1(_Name, <<"wtfpl">>) -> <<"wtfpl">>;
parse_license1(_Name, <<"the mit license">>) -> <<"mit">>;
parse_license1(Name, License) ->
    io:format("Unable to parse license ~s for ~s. Using "
              "'Unspecified free software license'~n", [License, Name]),
    <<"free">>.

%% @doc This exists to get around a rate limiter. Essentially, hex
%% limits us to one request per second. This allows us to do that.
-spec take_at_least_one_second(fun(() -> Result)) -> Result.
take_at_least_one_second(Thunk) ->
    Start = erlang:system_time(milli_seconds),
    Result = Thunk(),
    Now = erlang:system_time(milli_seconds),
    Wait = Now - Start,
    case Wait > 1000 of
        true ->
            Result;
        false ->
            timer:sleep(1000 - Wait),
            Result
    end.
