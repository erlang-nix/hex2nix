%%% ---------------------------------------------------------------------------
%%% @doc
%%%  Various utility functions used throughout the system.
%%% ---------------------------------------------------------------------------
-module(h2n_util).

-include_lib("ibrowse/include/ibrowse.hrl").

%%
%% API
%%
-export([stderr/2
        , binary_to_lower/1
        , iolist_to_list/1
        , json_to_assoc_list/1
        , json_get_assoc_list/2
        , json_to_list/1
        , json_get_list/2
        , temp_directory/0
        , run/1
        , cmd/1
        , run/2
        , cmd/2
        , get_ibrowse_http_env/0
        , consult/1
        , trim/1
        ]).

%% ============================================================================
%% Exported Functions
%% ============================================================================

%% @doc
%% This function gets environmental variables and translates them into
%% ibrowse-compatible options.
-spec get_ibrowse_http_env() -> [{atom(), term()}].
get_ibrowse_http_env() ->
    case os:getenv("http_proxy", "") of
        "" ->
            [];
        ProxyUrlString ->
            url_to_proxy_opts(ibrowse_lib:parse_url(ProxyUrlString))
    end.

-spec url_to_proxy_opts(#url{}) -> [{atom(), term()}].
url_to_proxy_opts(#url{host = Host
                      , port = Port
                      , username = Username
                      , password = Password}) ->
    maybe_empty_kv(proxy_host, Host)
        ++ maybe_empty_kv(proxy_port, Port)
        ++ maybe_empty_kv(proxy_user, Username)
        ++ maybe_empty_kv(proxy_password, Password).

-spec maybe_empty_kv(term(),  term()) -> [{term(), term()}].
maybe_empty_kv(_, undefined) ->
    [];
maybe_empty_kv(K, V) ->
    [{K, V}].

%% @doc
%% Write the result of the format string out to stderr.
-spec stderr(string(), [term()]) -> ok.
stderr(FormatStr, Args) ->
    io:put_chars(standard_error, io_lib:format(FormatStr, Args)).

-spec binary_to_lower(binary()) -> binary().
binary_to_lower(Value) ->
    erlang:list_to_binary(string:to_lower(erlang:binary_to_list(Value))).

-spec iolist_to_list(iolist()) -> list().
iolist_to_list(Value) ->
    erlang:binary_to_list(erlang:iolist_to_binary(Value)).

%% @doc
%% This mostly exists to make sure a value is actually a list from jsx
%% and to make dialyzer happy
-spec json_to_assoc_list(jsx:json_term()) ->
                                {ok, [{binary(), jsx:json_term()}]} | unconvertable.
json_to_assoc_list(Value) when erlang:is_list(Value) ->
    {ok, Value};
json_to_assoc_list(null) ->
    {ok, []};
json_to_assoc_list(Unconvertable) ->
    {unconvertable, Unconvertable}.

-spec json_get_assoc_list(binary(), jsx:json_term()) ->
                                 {ok, [{binary(), jsx:json_term()}]}
                                     | unconvertable
                                     | doesnt_exist.
json_get_assoc_list(Key, Object) ->
    case lists:keysearch(Key, 1, Object) of
        {value, {_, Value}} ->
            json_to_assoc_list(Value);
        false ->
            false
    end.


%% @doc
%% This mostly exists to make sure a value is actually a list from jsx
%% and to make dialyzer happy
-spec json_to_list(jsx:json_term()) -> {ok, [jsx:json_term()]} | unconvertable.
json_to_list(Value) when erlang:is_list(Value) ->
    {ok, Value};
json_to_list(_) ->
    unconvertable.

-spec json_get_list(binary(), jsx:json_term()) ->
                           {ok, [jsx:json_term()]}
                               | unconvertable
                               | doesnt_exist.
json_get_list(Key, Object) ->
    case lists:keysearch(Key, 1, Object) of
        {value, {_, Value}} ->
            json_to_assoc_list(Value);
        false ->
            false
    end.

-spec temp_directory() -> string().
temp_directory() ->
    lib:nonl(cmd("mktemp -d")).

-spec cmd(string()) -> string().
cmd(Cmd) ->
    {ok, Output} = run(Cmd),
    lists:flatten(Output).

-spec cmd(string(), [any()]) -> string().
cmd(CmdFormat, FormatArgs) ->
    {ok, Output} = run(CmdFormat, FormatArgs),
    lists:flatten(Output).

-spec run(string(), [any()]) -> {'ok', iolist()} | {'error', integer(), iolist()}.
run(CmdFormat, FormatArgs) ->
    run(io_lib:format(CmdFormat, FormatArgs)).

-spec run(string()) -> {'ok', iolist()} | {'error', integer(), iolist()}.
run(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    run_flush(Port, []).

-spec run_flush(port(), iolist()) -> {'ok', iolist()} | {'error', integer(), iolist()}.
run_flush(Port, Acc) ->
    receive
        {Port, {exit_status, 0}} ->
            {ok, lists:reverse(Acc)};
        {Port, {exit_status, Status}} ->
            {error, Status, lists:reverse(Acc)};
        {Port, {data, L}} ->
            run_flush(Port, [L|Acc])
    end.


%% @doc `file:consult` blows up in some unicode cases in r17. So we
%% had to copy this out and change the encoding to latin1. Thats not
%% great, but it gets us over the hump until things are fixed.
-spec consult(Filename) ->
                     {ok, Terms} | {error, Reason} when
      Filename :: file:name(),
      Terms :: [term()],
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
consult(File) ->
    case file:open(File, [read, {encoding, latin1}]) of
        {ok, Fd} ->
            R = consult_stream(Fd),
            file:close(Fd),
            R;
        Error ->    Error
    end.

-spec consult_stream(pid()) ->
                     {ok, Terms} | {error, Reason} when
      Terms :: [term()],
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
consult_stream(Fd) ->
    consult_stream(Fd, 1, []).

-spec consult_stream(pid(), integer(), [term()]) ->
                            {ok, Terms} | {error, Reason} when
      Terms :: [term()],
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
consult_stream(Fd, Line, Acc) ->
    case io:read(Fd, '', Line) of
        {ok,Term,EndLine} ->
            consult_stream(Fd, EndLine, [Term|Acc]);
        {error,Error,_Line} ->
                {error,Error};
        {eof,_Line} ->
                {ok,lists:reverse(Acc)}
    end.

-spec trim(binary()) -> binary().
trim(Bin) ->
    re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).
