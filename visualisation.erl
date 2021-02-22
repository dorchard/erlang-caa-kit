-module(visualisation).
-export([graph/1, quotation_check/1]).

graph({Start_State, [{From, Label, To}|Xs]}) ->
    % create graph
    {Graph, End_State} = createGraph([{From, Label, To}|Xs], "", ordsets:new(), ordsets:new()),
    CAA = io_lib:format("digraph Communicating_Actor_Automata {~n
            rankdir=LR;~n   size=\"100, 50\"~n  node [shape = doublecircle]; ~s~n
            node [shape = point]; START;~n   node [shape = circle];~n   START->~p;~n
            ~s~n}", [lists:concat(End_State), Start_State, Graph]),
    % io:fwrite(" The end states ~p~n", [End_States]),
    % create .dot file with Graph inside
    file:write_file("graph.dot", CAA),
    % installs Graphviz
    installGraphviz(os:type()),
    io:fwrite("For more information regrading Grapghviz, please visit their website: http://www.graphviz.org/download/ ~n"),
    % compiles the file and create a visual graph in pdf
    os:cmd("dot -Tpdf graph.dot > CAA.pdf"),
    % opens the file
    os:cmd("CAA.pdf"),
    done.

% for windows 
installGraphviz({win32, _}) ->
    os:cmd("choco install graphviz");

% for ubuntu os family
installGraphviz({ubuntu, _}) -> 
    % for linux
    os:cmd("sudo apt install graphviz"), % Ubuntu or Debain
    os:cmd("sudo yum install graphviz"), % Fedro or CebtOS systems or Redhat Enterprise
    % for mac
    os:cmd("sudo port install graphviz"),
    os:cmd("brew install graphviz").

createGraph([], Graph, From_s, To_s) ->
    % gets the end state, if not exist []
    End_State = ordsets:filter(fun(X) -> not ordsets:is_element(X, From_s) end, To_s),
    {Graph, End_State};
createGraph([{From, Label, To}|Xs], Graph, From_s, To_s) ->
    createGraph(Xs, io_lib:format( "~s~n~p -> ~p [ label = ~p];",
                    [Graph, From, To, quotation_check(Label)]), ordsets:add_element(integer_to_list(From), From_s),
                    ordsets:add_element(integer_to_list(To), To_s)).

% https://stackoverflow.com/questions/15534663/erlang-tuple-to-string
quotation_check(Label) ->
    lists:flatten(io_lib:format("~p", [Label])).
