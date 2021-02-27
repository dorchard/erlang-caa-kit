-module(visualisation).
-export([graph/1]).

-type caa() :: {start_State(), delta()}.
    % Communicating Actor Automata specifies
    % a tuple representation of Actor-based
    % model erlang code. The tuple contains
    % a staring state and it's delta.

-type start_State() :: integer().
    % start_State() represents the initial
    % state of the caa().

-type delta() :: [transition()].
    % delat() contains list of transitions
    % which represents the transition 
    % relations.

-type transition() :: {state(), label(), state()}.
    % transition represents a tuple containing
    % 3 elements, where the 1st element represents 
    % the start state() of the transtion, the 2nd 
    % element represents the label() and the 3rd 
    % element represents the next state after the
    % transition.

-type state() ::    integer()
               |    string().
    % state() represents the state which we at
    % during a transtion.

-type label() ::    {send, erl_parse:abstract_expr(), erl_parse:abstract_expr()}
                |   {recv, erl_parse:abstract_expr()}
                |   unlabelled
                |   undefined.
    % label() specifies the communication/label
    % over which the transition is happening.

-type os_type() :: os:type().
    % Returns the Osfamily and, in some cases, the
    % Osname of the current OS

-type graph() :: string().
    % graph() represents the caa()'s transitions/delta()
    % in form of grapgviz's (dot) transitions

-type state_ordsets() :: [state()].
    % state_ordsets() represents a set containing states().
    % It is used for containing all the from/start state of 
    % transitions and for contining all the next/to state of
    % those transitions (separately).

-type end_state() ::    []
                    |   [state()].
    % end_state() represents the end state of the caa(). If
    % end start not exist then []. 
     
-type prettyPrint_transition() :: string().
    % prettyPrint_transition() return the pretty print
    % transition version of caa() format transition()

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec graph(caa()) -> done.
    % the graph() method is responsible of:
    % 1) Converting the caa() into graphviz .dot
    %   format.
    % 2) Creating a .dot file name graph.dot
    %   which contains the graphviz .dot representaion
    %   of the caa().
    % 3) Intalling graphviz on your system if 
    %   required.
    % 4) Compiling the .dot file and extracting 
    %   a pdf file called CAA.pdf out of it,
    %   which represents the graph representation of
    %   the caa(). 
    % 5) And at the end the graph method opens the 
    %   CAA.pdf file for you.
    % 
    % Note: there can be a case where the method is
    % able to create a graph.dot file, but unable to
    % install the graphviz (if it's not intalled already)
    % which means it won't be able to create a CAA.pdf;
    % and it's due to security reason.
    % To get over this please visit the website:
    % http://www.graphviz.org/download/
    % and download the right graphviz software
    % in your system. And after dowloading it 
    % go to the repositorty where your graph.dot
    % file is in the command-line interface and
    % run the following command:
    % dot -Tpdf graph.dot > CAA.pdf
    % this will create the CAA.pdf file for you
    % on the same repository and then you can 
    % open it.

graph({Start_State, [{From, Label, To}|Xs]}) ->
    % create graph
    {Graph, End_State} = createGraph([{From, Label, To}|Xs], "", ordsets:new(), ordsets:new()),
    CAA = io_lib:format("digraph Communicating_Actor_Automata {~n
            rankdir=LR;~n   size=\"100, 50\"~n  node [shape = doublecircle]; ~s~n
            node [shape = point]; START;~n   node [shape = circle];~n   START->~p;~n
            ~s~n}", [lists:concat(End_State), Start_State, Graph]),
    % create .dot file with Graph inside
    file:write_file("graph.dot", CAA),
    % installs Graphviz
    installGraphviz(os:type()),
    io:fwrite("For more information regrading Grapghviz, please visit their website: http://www.graphviz.org/download/ ~n"),
    % compiles the .dot file and create a visual graph in pdf
    os:cmd("dot -Tpdf graph.dot > CAA.pdf"),
    % opens the file
    os:cmd("CAA.pdf"),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec installGraphviz(os_type()) -> install.
    % installs graphviz in you OS.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec createGraph(delta(), graph(), state_ordsets(), state_ordsets()) -> {graph(), end_state()}.

createGraph([], Graph, From_s, To_s) ->
    % gets the end state
    End_State = ordsets:filter(fun(X) -> not ordsets:is_element(X, From_s) end, To_s),
    {Graph, End_State};
createGraph([{From, Label, To}|Xs], Graph, From_s, To_s) ->
    createGraph(Xs, io_lib:format( "~s~n~p -> ~p [ label = ~p];",
                    [Graph, From, To, prettyPrint(Label)]), ordsets:add_element(integer_to_list(From), From_s),
                    ordsets:add_element(integer_to_list(To), To_s)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prettyPrint(transition()) -> prettyPrint_transition().

% https://stackoverflow.com/questions/15534663/erlang-tuple-to-string
% for lists:flatten(io:lib:format()) idea
% for send transition
prettyPrint({send, P_id, Data}) ->
    lists:flatten(io_lib:format("~s!~s", [erl_prettypr:format(P_id), erl_prettypr:format(Data)]));
% for receive transition
prettyPrint({recv, [Label]}) ->
    lists:flatten(io_lib:format("?~s", [erl_prettypr:format(Label)])).
