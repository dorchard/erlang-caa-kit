-module(eCFSM).
-export([main/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes the filename (with extension), method name and
% its arity. Returns its CAA form.
% example are provided in example.erl and toRead2.erl   
% c(eCFSM), rp(eCFSM:main("example.erl", "Methodname", Arity(int))).

main(Filename, Method, NumArgu) ->
    case parseToForm(Filename) of
        {error, OpenError} -> OpenError;
        Form -> case method_Form:getMethod(Form, list_to_atom(Method), NumArgu, Form, [Method ++ "->" ++ integer_to_list(NumArgu)]) of
                    error -> "No such method found";
                    Method_Form -> {CAA, State, _} = caa(Method_Form, #{}, 0, -1),
                        CAA
                end
end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% parse the file and return it's form

% https://erlang.org/doc/man/epp.html#parse_file-2
parseToForm(Filename) ->  
    case epp:parse_file(Filename, Filename) of
    {ok, Form} -> Form;
    {error, OpenError} -> {error, OpenError}
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% caa takes the form a method 
% ,MethodMap_State, a list containig all
% the last transition states and Pre assumed State
% And returns its CAA form
% and all the last transition states
% (the clause function)

caa({_, _, Method_Name, Arity, Clauses, Method_Term}, MethodState_Map, Last_Transition_State, Pre_assumedState) ->
    clause(Clauses, Method_Name, Arity, length(Clauses) > 1,
        false, maps:merge(MethodState_Map, #{Method_Term => Last_Transition_State}),
        0, [], [], [Last_Transition_State], Pre_assumedState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the clause methods takes the list containing the clause of 
%  the method ({clause, ANNO, Arity, Arrow, Clause_Body (body of the clause)},
% Method_Name, Arity, does this method have many clauses,
% we are at Nth clause of this method where N > 1,
%  map containing Method_Term and it's states,
            % last clause highest transition state,   %%%%%%%% Note might not need this parameter, coz the one after this be use by lists:max() 
% last transition states of all the clauses,
%  CAA,
% last transition states of the parent, pre assumed state)
clause([], _, _, _, _, _, Max_StateMethod, Clauses_Last_Transition_States, CAA, Parent_Last_Transition_States, Pre_assumedState) ->
    {{lists:max(Parent_Last_Transition_States), CAA}, Clauses_Last_Transition_States, Max_StateMethod};
% when there is only one clause in the method
clause([{_,_,_,_,Clause_Body}], Method_Name, Arity, false, false, MethodState_Map, _, _, _, Parent_Last_Transition_States, Pre_assumedState) ->
    MAX_CAA_State = lists:max(Parent_Last_Transition_States),
    {Delta, Clause_Last_Transition_States, NEW_Max_State, _} = 
        sequential(Clause_Body, Method_Name, Arity, MAX_CAA_State, MethodState_Map, [], Parent_Last_Transition_States, Pre_assumedState),
    {{MAX_CAA_State, Delta}, Clause_Last_Transition_States, NEW_Max_State};
% when there are many clauses in the method and
% this is the first clause
clause([{_,_,_,_,Clause_Body}|Xs], Method_Name, Arity, true, false, MethodState_Map, _, _, _, Parent_Last_Transition_States, Pre_assumedState) ->
    MAX_CAA_State = lists:max(Parent_Last_Transition_States),
    {Delta, ClauseLast_Transition_States, Max_StateClause, _} = 
        sequential(Clause_Body, Method_Name, Arity, MAX_CAA_State+1, MethodState_Map, [], [MAX_CAA_State+1], Pre_assumedState),
    case Delta == [] of
        false ->
            clause(Xs, Method_Name, Arity, true, true, MethodState_Map, Max_StateClause,
                ClauseLast_Transition_States,
                addTraces(Delta, Parent_Last_Transition_States, MAX_CAA_State, undefined),
                Parent_Last_Transition_States, Pre_assumedState);
        _ -> % when there was just a recursive call inside this clause
            clause(Xs, Method_Name, Arity, true, true, MethodState_Map, MAX_CAA_State,
                [],
                addTraces(Delta, Parent_Last_Transition_States, MAX_CAA_State-1, unlabelled),
                Parent_Last_Transition_States, Pre_assumedState)
end;
% when there are many clauses in the method and
% this is the Nth clause where N > 1
clause([{_,_,_,_,Clause_Body}|Xs], Method_Name, Arity, true, true, MethodState_Map, Max_State_lastClause,
    Clauses_Last_Transition_States, CAA, Parent_Last_Transition_States, Pre_assumedState) ->
        MAX_CAA_State = lists:max(Parent_Last_Transition_States),
        {Delta, ClauseLast_Transition_States, New_Max_StateClause, _} =
            sequential(Clause_Body, Method_Name, Arity, Max_State_lastClause+1, MethodState_Map, [], [Max_State_lastClause+1], Pre_assumedState),
        case Delta == [] of
            false ->
                clause(Xs, Method_Name, Arity, true, true, MethodState_Map, New_Max_StateClause,
                ClauseLast_Transition_States ++ Clauses_Last_Transition_States,
                CAA ++ addTraces(Delta, Parent_Last_Transition_States, MAX_CAA_State, undefined),
                Parent_Last_Transition_States, Pre_assumedState);
            _ ->  % when there was just a recursive call inside this clause
                clause(Xs, Method_Name, Arity, true, true, MethodState_Map, Max_State_lastClause,
                Clauses_Last_Transition_States,
                CAA ++ addTraces(Delta, Parent_Last_Transition_States, MAX_CAA_State-1, unlabelled),
                Parent_Last_Transition_States, Pre_assumedState)
end.

%%%%%%%%%%%%%%%%%%%%%%%%

% (Expression, Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States,  Last_Pre_assumedState) 
% sequential works on sequential part of the code 

sequential([],_,_,Max_State,_,Delta, Last_Transition_States, _) ->
    % In the tupe the last element represent that there was no recursion in this method
   {lists:reverse(Delta), Last_Transition_States, Max_State, true}; 

% when there is a send in the clause
sequential([{_,_,'!',Process_ID,Data}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, Last_Pre_assumedState) ->
    sequential(Xs, Method_Name, Arity, Max_State+1, MethodState_Map,
        addTraces(Delta, Last_Transition_States, Max_State, {send, Process_ID, Data}),
            [Max_State+1],  Last_Pre_assumedState);

% When there is a recursion i.e. call expression
sequential([{call,_,{_,_,_}, _, Method_Term}|_], _, _, Max_State, MethodState_Map, Delta, Last_Transition_States, _) -> 
    Call_MethodState = maps:get(Method_Term, MethodState_Map), % get the method starting state
    case Delta == [] of
                % when there was no send and receive before
                % and now it encounters recursion
                true -> {[], Last_Transition_States, Max_State, false};
                % when there was send and receive before
                % this recursion call
                _ -> changeTracesStates(Delta, Last_Transition_States, [], Call_MethodState, MethodState_Map, [])
end;

sequential([{'receive', _, Body}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States,  Last_Pre_assumedState) ->
    case Xs == [] andalso  Last_Pre_assumedState =/= -1 of
        % when the there is nothing after this receive block
        % and this is a nested receive block. look at example "recv6(S, Z)"
        % 2nd receive nested receive block
        true -> Pre_assumedState = Last_Pre_assumedState;
        % when this is either the very first receive block or there are 
        % other stuff after this receive block. look at example "recv(S, Z)"
        % or "recv6(S, Z)" first recive nested receive block
        _ -> Pre_assumedState = Max_State + 1 % the starting state of the next expression after this.
    end,
    {Recv_Delta, Recv_Max_State} = 
        receive_block(Body, Pre_assumedState, Max_State + 1, Method_Name, Arity, MethodState_Map, Delta, Last_Transition_States),
    sequential(Xs, Method_Name, Arity, Recv_Max_State, MethodState_Map, Recv_Delta, [Pre_assumedState], Last_Pre_assumedState);

sequential([{function, Anno, CallMethod_Name, CallArity, Clauses, Method_Term}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States,  Last_Pre_assumedState) ->
    case caa({function, Anno, CallMethod_Name, CallArity, Clauses, Method_Term}, MethodState_Map, lists:nth(1, Last_Transition_States), Last_Pre_assumedState) of
        {Func_Delta, Func_Last_Transition_States, Func_Max_State} -> % when there was no recursion inside this child method
            sequential(Xs, Method_Name, Arity, Func_Max_State, MethodState_Map, lists:reverse(Func_Delta) ++ Delta , Func_Last_Transition_States,  Last_Pre_assumedState);
        {Func_Delta, Func_Last_Transition_States, Func_Max_State} -> % when there was recursion inside this child method
            {lists:reverse(Delta) ++ Func_Delta, Func_Last_Transition_States, Func_Max_State, false}
    end;

% when none of the above
sequential([X|Xs], Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, Last_Pre_assumedState) -> 
    sequential(Xs, Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, Last_Pre_assumedState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% deals with the clauses of a receive block
% ([{'caluse', Anno, [receive], [], [expressions]}|Xs],
% pre-assumedState, the last receive clause max state,
% Method name, Method arity, MethodState_Map, delta, last transition state,
% or the starting state of the receive block)
receive_block([], _, LastClause_Max_State, _, _, _, Delta, _) ->
    {Delta, LastClause_Max_State};
receive_block([{_, _, Recv, _, Body}|Xs], Pre_assumedState, LastClause_Max_State, Method_Name, Arity, MethodState_Map, Delta, Last_Transition_States) ->
    {Recv_Delta, _, Recv_Max_State, _ } = 
        sequential(Body, Method_Name, Arity, LastClause_Max_State+1, MethodState_Map,
            addTraces(Delta, Last_Transition_States, LastClause_Max_State, {recv, Recv}),
                [LastClause_Max_State+1], Pre_assumedState),
    Pre_assumedState_Delta = 
        recv_changeTracesStates(lists:reverse(Recv_Delta), Pre_assumedState, MethodState_Map),
    receive_block(Xs, Pre_assumedState, Recv_Max_State, Method_Name, Arity, MethodState_Map, Pre_assumedState_Delta, Last_Transition_States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% changes the max state of the receive clause to the pre assumed state
% delta, pre-assumed state, MethodState_Map

recv_changeTracesStates([{First_State, Label, Last_State}|Xs], Pre_assumedState, MethodState_Map) ->
    case lists:member(Last_State, maps:values(MethodState_Map)) of
        % if the last state is not representing a recursion
        false   ->  [{First_State, Label, Pre_assumedState}|Xs];
        _       ->  [{First_State, Label, Last_State}|Xs]
end.      

%%%%%%%%%%%%%%%%%%%%%%%%%%

% (delta/traces, Last Transition States, The Max state in 'Last Transition States', the new transition label)
% this add traces in delta

addTraces(Delta, [], _, _) ->
    Delta;
addTraces(Delta, [X|Xs], Max_State, Label) -> 
    addTraces([{X, Label, Max_State + 1}|Delta], Xs, Max_State, Label).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (delta/traces, Last Transition States, new Delta, the method call state,
%  Map storing the Methods and their states, new last transition states after recursion)

% this changes the transtion state of last traces that happened 
% before the recursion call 

changeTracesStates([], _, Delta, _, _, New_Last_Transition_States) ->
    {Delta, New_Last_Transition_States, lists:max(New_Last_Transition_States), false};
changeTracesStates([{First_State, Label, Last_State}|Xs], Last_Transition_States, Delta, Method_Call_State, MethodState_Map, New_Last_Transition_States) ->
    case lists:member(Last_State, Last_Transition_States) andalso lists:member(Last_State, maps:values(MethodState_Map)) == false of
        % when the current trace is one of the last transtions trace and the transition state 
        % of this trace is not representing recursion coz it can inside a receive block
        true -> changeTracesStates(Xs, Last_Transition_States, [{First_State, Label, Method_Call_State}|Delta], Method_Call_State, MethodState_Map, [First_State|New_Last_Transition_States]);
        _    ->  changeTracesStates(Xs, Last_Transition_States, [{First_State, Label, Last_State}|Delta], Method_Call_State, MethodState_Map, New_Last_Transition_States)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
