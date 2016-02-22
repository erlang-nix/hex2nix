%%% ---------------------------------------------------------------------------
%%% @doc
%%%
%%%  This module actually generates the final file. It uses John
%%%  Hughes' brilliant `prettypr` module to output nice, human
%%%  readable Nix expressions
%%%  ---------------------------------------------------------------------------
-module(h2n_generate).

%%
%% API
%%
-export([nix_expression/2]).

-export([format_name/1, format_name/2]).

-include("hex2nix.hrl").
%%
%% Types
%%

%% We almost never do this, however, with `prettypr` it actually makes
%% some modicum of sense.
-import(prettypr, [above/2, beside/2, sep/1, par/1, break/1,
                   par/2, empty/0]).

%% ============================================================================
%% Exported Functions
%% ============================================================================
-spec nix_expression([h2n_fetcher:dep_desc()],
                     [h2n_fetcher:dep_desc()]) -> string().
nix_expression(Deps0, Failing) ->
    Deps1 = sort_and_dedup_deps(Deps0),
    Doc = above(above(header(Failing),
                      nest(par([sep([text("packages"), text("="), text("self: rec {")])
                               , nest(create_body(Deps1))
                               , break(text("};"))]))),
                text("in stdenv.lib.fix' (stdenv.lib.extends overrides packages)")),
    Pretty = prettypr:format(Doc),
    Pretty1 = re:replace(Pretty, "\t", "        ", [global]),
    re:replace(Pretty1, "\\h+\\n", "\n", [global]).

%% ============================================================================
%% Internal Functions
%% ============================================================================
-spec sort_and_dedup_deps([h2n_fetcher:dep_desc()]) -> [h2n_fetcher:dep_desc()].
sort_and_dedup_deps(Deps) ->
    lists:usort(fun(#dep_desc{app=App1}, #dep_desc{app=App2}) ->
                       App1 =< App2
               end, Deps).

-spec create_body([h2n_fetcher:dep_desc()]) -> prettypr:document().
create_body(Deps) ->
    par(lists:map(fun create_dep/1, Deps)).

-spec create_dep(h2n_fetcher:dep_desc()) -> prettypr:document().
create_dep(Dep = #dep_desc{app = {Name, Vsn}
                          , position = Position
                          , deps = Deps0}) ->
    NixName = format_name(Name, Vsn),
    Deps1 = lists:map(fun({DepName, DepVsn}) ->
                             format_name(DepName, DepVsn)
                     end, Deps0),
    above(blank_line(above(sep([text(NixName), text("="), text("callPackage")])
                          , nest(par([text("(")
                                     , nest(par([section_header(Deps1)
                                                , nest(app_body(Dep, Deps1))]))
                                     , text(") {};")])))),
          format_position(Name, NixName, Position)).

-spec format_position(binary(), binary(), h2n_fetcher:position()) ->
                             prettypr:document().
format_position(Name, NixName, root) ->
    blank_line(follow(text([Name, " ="]), text([NixName, ";"])));
format_position(_, _, _) ->
    empty().

-spec app_body(h2n_fetcher:dep_despc(), [binary()]) -> prettypr:document().
app_body(Dep = #dep_desc{build_plugins = BuildPlugins}, Deps) ->
    erlang_deps(Deps),
    par([break(sep([text("buildRebar3"), text("{")]))
        , nest(par([src(Dep)
                   , format_compile_port(Dep)
                   , build_plugins(BuildPlugins)
                   , erlang_deps(Deps)
                   , meta(Dep)]))
        , text("}")]).

-spec format_compile_port(h2n_fetcher:dep_desc()) -> prettypr:document().
format_compile_port(#dep_desc{has_native_code = true}) ->
    break(follow(text(["compilePorts", " ="]), text("true;")));
format_compile_port(_) ->
    empty().

-spec src(h2n_fetcher:dep_despc()) -> prettypr:document().
src(#dep_desc{app = {Name, Vsn},
                         sha = Sha}) ->
    par([sep([text("src")
             , text("=")
             , text("fetchHex")
             , text("{")])
        , nest(par([key_value(<<"name">>, Name)
                   , key_value(<<"version">>, Vsn)
                   , key_value(<<"sha256">>, Sha)]))
        , text("};")]).

-spec meta(h2n_fetcher:dep_despc()) -> prettypr:document().
meta(#dep_desc{description = Description
              , licenses = Licenses
              , homepage = Homepage}) ->
    format_licenses(Licenses),
    par([sep([text("meta")
             , text("=")
             , text("{")])
        , nest(par([format_description(Description)
                   , format_licenses(Licenses)
                   , case Homepage of
                         no_source -> empty();
                         _ -> key_value(<<"homepage">>, Homepage)
                     end]))
        , text("};")]).

-spec nix_escape_string(binary()) -> binary().
nix_escape_string(S) ->
    re:replace(S, "(\\\\|\"|\\${)", "\\\\&", [global, {return, binary}]).

-spec format_description(binary()) -> prettypr:document().
format_description(<<"">>) ->
    empty();
format_description(Description)
  when erlang:byte_size(Description) =< 80 ->
    key_value(<<"description">>, nix_escape_string(Description));
format_description(Description) ->
    DescBody = ["\"", nix_escape_string(Description), "\";"],
    break(follow(text(["longDescription", " ="])
                , text_par(DescBody))).


-spec format_licenses([binary()]) -> prettypr:document().
format_licenses([]) ->
    empty();
format_licenses([License]) ->
    sep([text("license"), text("="), text(["stdenv.lib.licenses.", License, ";"])]);
format_licenses(Values) ->
    sep([text("license"), text("="), text("with")
        , text("stdenv.lib.licenses;")
        , text("[")
        , nest(expand_arg_list(Values, "", []))
        , text("];")]).

-spec build_plugins([binary()]) -> prettypr:document().
build_plugins(Deps) ->
    key_list("buildPlugins", Deps).

-spec erlang_deps([binary()]) -> prettypr:document().
erlang_deps(Deps) ->
    key_list("erlangDeps", Deps).

-spec key_list(iolist(), [binary()]) -> prettypr:document().
key_list(_Key, []) ->
    empty();
key_list(Key, Deps) ->
    blank_line(follow(sep([text(Key), text(" =")])
                     , par([text("[")
                           , nest(expand_arg_list(Deps, "", []))
                           , text("];")]))).

-spec key_value(binary(), binary()) -> prettypr:document().
key_value(Key, Value) ->
    key_value_sep(Key, Value, "\"").

-spec key_value_sep(binary(), binary(), string()) -> prettypr:document().
key_value_sep(Key, Value, Sep) ->
    break(follow(text([Key, " ="]), text([Sep, Value, Sep, ";"]))).

-spec section_header([binary()]) -> prettypr:document().
section_header(Deps) ->
    sep([text("{ ")
        , nest(expand_arg_list([<<"buildRebar3">>,
                                <<"fetchHex">> | Deps], ",", []))
        , text("}:")]).

-spec expand_arg_list([binary()], string(), [prettypr:document()]) ->
                             prettypr:document().
expand_arg_list([], _Sep, _Acc) ->
    empty();
expand_arg_list([Name], _Sep, Acc) ->
    sep(lists:reverse([text([Name]) | Acc]));
expand_arg_list([Name | Tail], Sep, Acc) ->
    expand_arg_list(Tail, Sep, [text([Name, Sep]) | Acc]).

-spec format_name(hex2nix:dep_desc() |
                  {hex2nix:app_name(), hex2nix:app_version()}) ->
                         binary().
format_name(#dep_desc{app = App}) ->
    format_name(App);
format_name({Name, Vsn}) ->
    format_name(Name, Vsn).

-spec format_name(hex2nix:app_name(), hex2nix:app_version()) ->
                         binary().
format_name(Name, Vsn) ->
    erlang:iolist_to_binary([Name, "_", convert_vsn_to_name_part(Vsn)]).

-spec header([h2n_fetcher:dep_desc()]) -> prettypr:document().
header(Failing) ->
    Header = [text("/* hex-packages.nix is an auto-generated file -- DO NOT EDIT! */")
             , text("")
             , list_failing(Failing)
             , blank_line(text("{ stdenv, callPackage, overrides ? (self: super: {}) }:"))
             , text("let")],
    vertical_list(Header).

-spec list_failing([h2n_fetcher:dep_desc()]) -> prettypr:document().
list_failing(Failing) ->
    Names = lists:map(fun(X) ->
                              text([" * ", format_name(X)])
                      end, Failing),
    vertical_list([text("/* Unbuildable packages: "),
                   text("")]
                  ++ Names
                  ++ [text(""),
                      text("*/")]).

-spec vertical_list([prettypr:document()]) -> prettypr:document().
vertical_list([H|L]) ->
    lists:foldl(fun(Doc, Acc) ->
                        above(Acc, Doc)
                end, H,  L).

%% @doc
%% Produces a blank line after the provided document
-spec blank_line(prettypr:document()) -> prettypr:document().
blank_line(Doc) ->
    break(above(Doc
               , text(""))).

-spec nest(prettypr:document()) -> prettypr:document().
nest(Document) ->
    prettypr:nest(2, Document).

-spec follow(prettypr:document(), prettypr:document()) -> prettypr:document().
follow(Doc1, Doc2) ->
    prettypr:follow(Doc1, Doc2, 2).

-spec text(binary() | iolist()) -> prettypr:document().
text(Data)
  when erlang:is_binary(Data)->
    prettypr:text(erlang:binary_to_list(Data));
text(Data)
  when erlang:is_list(Data) ->
    prettypr:text(erlang:binary_to_list(erlang:iolist_to_binary(Data))).

-spec text_par(iodata()) -> prettypr:document().
text_par(Data)
  when erlang:is_list(Data) ->
    prettypr:text_par(erlang:binary_to_list(erlang:iolist_to_binary(Data))).

-spec convert_vsn_to_name_part(hex2nix:app_version()) -> iolist().
convert_vsn_to_name_part(Vsn) ->
    format_semver(ec_semver:parse(Vsn)).

-spec format_semver(ec_semver:semver()) -> iolist().
format_semver({Maj, {AlphaPart, BuildPart}})
  when erlang:is_integer(Maj);
       erlang:is_binary(Maj) ->
    [format_semver_version_part(Maj)
    , format_semver_vsn_rest(<<"_">>, AlphaPart)
    , format_semver_vsn_rest(<<"_">>, BuildPart)];
format_semver({{Maj, Min}, {AlphaPart, BuildPart}}) ->
    [format_semver_version_part(Maj), "_"
    , format_semver_version_part(Min)
    , format_semver_vsn_rest(<<"_">>, AlphaPart)
    , format_semver_vsn_rest(<<"_">>, BuildPart)];
format_semver({{Maj, Min, Patch}, {AlphaPart, BuildPart}}) ->
    [format_semver_version_part(Maj), "_"
    , format_semver_version_part(Min), "_"
    , format_semver_version_part(Patch)
    , format_semver_vsn_rest(<<"_">>, AlphaPart)
    , format_semver_vsn_rest(<<"_">>, BuildPart)];
format_semver({{Maj, Min, Patch, MinPatch}, {AlphaPart, BuildPart}}) ->
    [format_semver_version_part(Maj), "_"
    , format_semver_version_part(Min), "_"
    , format_semver_version_part(Patch), "_"
    , format_semver_version_part(MinPatch)
    , format_semver_vsn_rest(<<"_">>, AlphaPart)
    , format_semver_vsn_rest(<<"_">>, BuildPart)].

-spec format_semver_version_part(integer() | binary()) -> iolist().
format_semver_version_part(Vsn)
  when erlang:is_integer(Vsn) ->
    erlang:integer_to_list(Vsn);
format_semver_version_part(Vsn)
  when erlang:is_binary(Vsn) ->
    Vsn.

-spec to_list(integer() | binary() | string()) -> string() | binary().
to_list(Detail) when erlang:is_integer(Detail) ->
    erlang:integer_to_list(Detail);
to_list(Detail) when erlang:is_list(Detail); erlang:is_binary(Detail) ->
    Detail.

-spec format_semver_vsn_rest(binary() | string(), [integer() | binary()]) -> iolist().
format_semver_vsn_rest(_TypeMark, []) ->
    [];
format_semver_vsn_rest(TypeMark, [Head | Rest]) ->
    [TypeMark, Head |
     [["_", to_list(Detail)] || Detail <- Rest]].
