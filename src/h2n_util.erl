%%% ---------------------------------------------------------------------------
%%% @doc
%%%  Various utility functions used throughout the system.
%%% ---------------------------------------------------------------------------
-module(h2n_util).


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
        ]).

%% ============================================================================
%% Exported Functions
%% ============================================================================
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
json_to_assoc_list(_) ->
    unconvertable.

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
    lib:nonl(os:cmd( "mktemp -d")).
