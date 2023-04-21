-module(generate_code).
-export([main/1]).

%%TODO:
%% 1) Export first (Done)
%% 2) Add leaf nodes (Done)
%% 3) Parameters!

-spec main([type:caa()]) -> [erl_parse:abstract_clause()].

%% Program entry point
%% @param a list of CAAs
%% @return a list of AST, each item in the list, represents the AST for a CAA.
main([]) -> [];
main([CAA | CAAs]) -> printAll(analyzeCAA(CAA)), main(CAAs).

%% Step function that returns an AST for a specific AST
%% @param A CAA
%% @return An AST corresponding to the passed CAA.
analyzeCAA({Initial, Transactions}) -> (generateAST(Initial,getFuncsAndActions(Transactions,[]),Transactions)).


%% TODO: Add module attribute, add attributes to list first.
%% This function generates abstract forms for attributes and functions separately
%% @param The initial state - to know what function to export
%% @param A list of functions (TODO: Define structure of a function)
%% @return An AST corresponding to the passed arguments.
generateAST(Initial,[], Transactions) -> (exportFlagToAST(Initial)) ++ unique(sets:to_list(sets:from_list((emptyFuncs(Transactions,Transactions)))));
generateAST(Initial,[F | Fs], Transactions) -> generateAST(Initial, Fs, Transactions) ++ [funcToAST(F,Transactions)].

%% Generates a list of AST of empty functions (final states)
%% @param Transactions - list of transactions to go through
%% @param Transactions - list of transactions to check if isEmpty or no
%% @return List of ASTs with empty functions.
emptyFuncs([], _) -> [];
emptyFuncs([T | Ts], Transactions) ->
    %io:fwrite("~w    ~w \n", [T,Transactions]),
    case isEmpty(T,Transactions) of
      true -> [getName(T)] ++ emptyFuncs(Ts, Transactions);
      false -> emptyFuncs(Ts, Transactions)
end.

%% Goes through a list of names and generates empty functions in string form
%% @param list of names
%% @return list of strings
unique([]) -> [];
unique([Name | Names]) -> [generateEmpty(Name)] ++ unique(Names).

%% Returns name of end state
%% @param Transition
%% @return Name of end state
getName({_,_,Name}) -> Name.

%% If a function has no no actions coming out of it (no children) - it's a leaf
%% @param A specific transactions to analyse
%% @param List of transactions.
%% @return boolean - is it leaf or not.
isEmpty({State, _ , State}, []) -> false;
isEmpty({_, _, _State}, []) -> true;
isEmpty({NotSame, NotSame2, State}, [{NotSame, NotSame2, State} | Ts]) -> isEmpty({NotSame, NotSame2, State}, Ts);
isEmpty({_,_,State}, [{State,_,_} | _]) -> false;
isEmpty({S, Stuff, State}, [_ | Ts]) -> isEmpty({S, Stuff, State}, Ts).

%% Generates an AST of an empty function
%% @param Transition with name.
%% @return An AST representing an empty function
generateEmpty(Name) -> forms:to_abstract(generateEmptyFunc(Name)).

%% Generates the string version of an empty function.
%% @param Name - name of the function
%% @return String representing an empty function.
generateEmptyFunc(Name) -> "f" ++ integer_to_list(Name) ++ "() -> finish.".

%% Generates the abstract version of the export attribute
%% @param Name of the exxporting function(starting state)
%% @return An AST representing the export attribute
exportFlagToAST(Name) -> [forms:to_abstract("-export([f" ++ integer_to_list(Name) ++ "/0]).")].

%% This function generates an AST for a function.
%% A function is formed of 2 things: the header and its instructions. We get the strings for each of them and transform them to syntax forms.
%% @param Function name, and an action list, where each 'action' in the the list is represented by an action type + the end state in which the program
%% will be after executing the instruction.
%% @return An AST of a function.
funcToAST({Name, Actions}, Transitions) -> forms:to_abstract(generateHeader(Name,getAllParams(Name,getFuncsAndActions(Transitions, []),Transitions,Transitions)) ++ actionsToString(Actions,Transitions) ++ " ok.").

%%CARE: Second call getAllParams, we might need to pass Transitions, not Ts.
%% This functions returns all parameters needed for a function to work (including it's called functions and so on).
%% @param Function name
%% @param A list of function actions.
%% @param A list of transitions to go through.
%% @param A list of transitions.
%% @return A list of parameters.
getAllParams(_,_,[], _) -> [];
getAllParams(Name,Functions,[{Name,_,EndState} | Ts], Transitions) -> findParamsIn(Name,Functions) ++
                              getAllParams(EndState,Functions,Ts,Transitions) ++
                              getAllParams(Name, Functions, Ts, Transitions);
getAllParams(Name,Functions,[_ | Ts], Transitions) -> getAllParams(Name,Functions,Ts,Transitions).


%% TODO: Add the params the function needs (processes ids)
%% Generates a string corresponding to a function header.
%% @param function name
%% @return string of function header
generateHeader(Name,Params) -> "f" ++ integer_to_list(Name) ++ "(" ++ paramsToString(Params) ++ ") -> ".


%% A function that transform a list of parameters into a string format.
%% @param List of parameters.
%% @return Returns a string representation of the parameters.
paramsToString(Params) -> string:strip(addCommas(sets:to_list(sets:from_list(Params))),both,$,).

%% Addes commas to the representation.
%% @param List of atoms that represent the parameters
%% @return String representation of parameters with commas between them.
addCommas([]) -> "";
addCommas([P | Ps]) -> toString(P) ++ "," ++ addCommas(Ps).

%% Transforms an action to string
%% If the action is a send, the second function header is called, otherwise the third.
%% @param An action list, where each 'action' in the the list is represented by an action type + the end state in which the program
%% @return a string of all the actions converted to Erlang instructions.
actionsToString([],_) -> "";
actionsToString([{{send,P,S} , End} | _],Transitions) -> toString(P) ++ " ! " ++ toString(S) ++ ", f" ++ integer_to_list(End) ++ "(" ++ paramsToString(getAllParams(End, getFuncsAndActions(Transitions, []), Transitions, Transitions)) ++ "), ";
actionsToString(Receives,Transitions) ->
  "receive " ++ receiveToString(Receives,Transitions) ++ "end,".

%% Transforms receive actions to string
%% @param a list of receive actions
%% @return a string corresponding to receive clauses.
receiveToString([],_) -> "";
receiveToString([{{recv,A}, End} | [R | Rs]], Transitions) -> toString(A) ++ " -> " ++ " f" ++ integer_to_list(End) ++ "(" ++ paramsToString(getAllParams(End, getFuncsAndActions(Transitions, []), Transitions, Transitions)) ++ ");" ++ receiveToString([R] ++ Rs, Transitions);
receiveToString([{{recv,A}, End} | []], Transitions) -> toString(A) ++ " -> " ++ " f" ++ integer_to_list(End) ++ "(" ++ paramsToString(getAllParams(End, getFuncsAndActions(Transitions, []), Transitions, Transitions)) ++ ") ".

%% Builds a list of function strucures.
%% @param Transition list.
%% @param Current list of function structures.
%% @return Complete list of function structures.
getFuncsAndActions([],R) -> R;
getFuncsAndActions([T | Ts], R) -> getFuncsAndActions(Ts, addTrans(T, R)).

%% Adds a transition to the transition list.
%% @param Transition to be added
%% @param Current list of function structures.
%% @return updated list of function structures.
addTrans({Start,Action,End}, [])-> [{ Start, [ {Action,End} ] }];
addTrans({Start,Action,End}, [ {Name, Actions} | Rs]) when Name == Start -> [{Name, Actions ++ [{Action,End}]}] ++ Rs;
addTrans({Start,Action,End}, [ {Name, Actions} | Rs ])-> [{Name,Actions}] ++ addTrans({Start,Action,End}, Rs).

%% Takes some expression and returns the string version of it.
%% @param Some variable/atom
%% @return String representation of the parameter
%% Note: For tuple, we assume that we get an erlang AST always.
%%       Then we use the great forms module to convert to String.
toString(S) when is_atom(S) == true -> atom_to_list(S);
toString(S) when is_integer(S) == true -> integer_to_list(S);
toString(S) when is_tuple(S) == true -> forms:from_abstract(S);
toString(S) -> S.

%% Test method to print an AST.
printAll([]) -> io:format("\n");
printAll([X | Xs]) -> io:format("~s\n", [erl_prettypr:format(X)]), printAll(Xs).

%% This method returns a list of parameters found in a function.
%% @param Name of function
%% @param A list of functions and their next function call.
%% @return A list of parameters the function uses.
findParamsIn(_,[]) -> [];
findParamsIn(Name,[{Name,ActionsAndMove} | _]) -> findParamsInTransitions(ActionsAndMove);
findParamsIn(Name,[_ | Fs]) -> findParamsIn(Name,Fs).

%% This function finds a list of parameters found in a function.
%% @param Actions a functions take
%% @return A list of parameters the function uses.findParamsInTransitions([]) -> [];
findParamsInTransitions([]) -> [];
findParamsInTransitions([T | Ts]) -> findParamsInTransition(T) ++ findParamsInTransitions(Ts).

%% This function finds a list of parameters in a Transition.
%% @param Transition to look at for parameters.
%% @return A list of parameter found in the transition.
findParamsInTransition({{recv, _}, _}) -> [];
findParamsInTransition({{send,SendTo,Message},_}) -> fromAbstractParams(SendTo) ++ fromAbstractParams((Message));
findParamsInTransition(_) -> [].

%% This function extracts parameters from their abstract form.
%% @param AST representation.
%% @return A list of Parameters found in the abstract representation.
fromAbstractParams({var,_,X}) -> [X];
fromAbstractParams({tuple,_,List}) -> fromAbstractParams(List);
fromAbstractParams([]) -> [];
fromAbstractParams([P | Ps]) -> fromAbstractParams(P) ++ fromAbstractParams(Ps);
fromAbstractParams(_) -> [].
