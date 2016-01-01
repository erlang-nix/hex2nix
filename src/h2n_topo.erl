%%% ---------------------------------------------------------------------------
%%% @doc
%%%  This serves to [Topological Sort]
%%%  (https://en.wikipedia.org/wiki/Topological_sorting) This allows
%%%  us to put the dependencies of an application near the application
%%%  itself. Its simply a decoration for the generated file.
%%%  ---------------------------------------------------------------------------
-module(h2n_topo).


%%
%% API
%%
-export([sort/1]).

%% ============================================================================
%% Exported Functions
%% ============================================================================
-spec sort([{hex2nix:app(), [hex2nix:app()]}]) -> [hex2nix:app()].
sort(Dependencies) ->
    Graph = digraph:new(),
    build_graph(Dependencies, Graph),
    case digraph_utils:topsort(Graph) of
        false ->
            print_cycle(Graph),
            erlang:throw(cycle_detected_in_dependency_graph);
        Result ->
            Result
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================
-spec build_graph(any(), digraph:graph()) -> ok.
build_graph(Dependencies, Graph) ->
    lists:foreach(fun ({App, Deps}) ->
                          digraph:add_vertex(Graph, App),
                          lists:foreach(fun (Dep) ->
                                                add_dependency(Graph, App, Dep)
                                        end, Deps)
                      end, Dependencies).

-spec add_dependency(digraph:graph(), hex2nix:app(), hex2nix:app()) ->
                            digraph:graph().
add_dependency(_Graph, App, App) ->
    ok;
add_dependency(Graph, App, Dep) ->
    digraph:add_vertex(Graph, Dep),
    digraph:add_edge(Graph, Dep, App).

-spec print_cycle(digraph:graph()) -> ok.
print_cycle(Graph) ->
    io:format("Unsortable contains circular dependencies:~n",[]),
    lists:foreach(fun (Vertex) ->
                          case digraph:get_short_cycle(Graph, Vertex) of
                              false ->
                                  ok;
                              Vertexes ->
                                  print_path(Vertexes)
                          end
                  end, digraph:vertices(Graph)).

-spec print_path([hex2nix:app()]) -> ok.
print_path(L) ->
            lists:foreach(fun (V) -> io:format("~s -> ",[V]) end,
                          lists:sublist(L,length(L)-1)),
            io:format("~s~n",[lists:last(L)]).
