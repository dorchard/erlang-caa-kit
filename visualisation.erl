-module(visualisation).
-export([graph/1]).

graph({Start_State, [{From, Label, To}|Xs]}) ->
    % create graph
    {Graph, End_States} = createGraph([{From, Label, To}|Xs], [ From, " "], ""),
    CAA = io_lib:format("digraph Communicating_Actor_Automata {~n
            rankdir=LR;~n   size=\"100, 50\"~n  node [shape = doublecircle]; ~s;~n
            node [shape = point]; START;~n   node [shape = circle];~n   START->~p;~n
            ~s~n}", [lists:concat(End_States), Start_State, Graph]),
    % io:fwrite(" The end states ~p~n", [End_States]),
    % create .dot file with Graph inside
    file:write_file("graph.dot", CAA),
    % installs Graphviz
    installGraphviz(os:type()),
    io:fwrite("For more informatioon regrading Grapghviz, please visit there website: http://www.graphviz.org/download/ ~n"),
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

createGraph([], End_States, Graph) ->
    {Graph, End_States};
createGraph([{From, Label, To}|Xs], End_States, Graph) ->
    case lists:member(From, End_States) of 
        % i.e., if this state "From" is now having a transition then it cannot be a End state 
        false ->createGraph(Xs, [To, " "] ++ End_States, 
                    io_lib:format( "~s~n~p -> ~p [ label = \"~p\"];", [Graph, From, To, Label]));
        _   ->  createGraph(Xs, [To, " "] ++ lists:delete(From, End_States), 
                    io_lib:format( "~s~n~p -> ~p [ label = \"~p\"];", [Graph, From, To, Label]))
end.

    