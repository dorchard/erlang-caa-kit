-module(eCFSM).
-export([main/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes the filename (with extension), method name and
% its arity. Returns its CAA form.
% example are provided in example.erl and toRead2.erl   
% c(eCFSM), eCFSM:main("example.erl", "Methodname", Arity(int)).

main(Filename, Method, NumArgu) ->
    compile:file("visualisation"),
    compile:file("method_Form"),
    case parseToForm(Filename) of
        {error, OpenError} -> OpenError;
        Form -> case method_Form:getMethod(Form, list_to_atom(Method), NumArgu, Form, [Method ++ "->" ++ integer_to_list(NumArgu)]) of
                    error -> "No such method found";
                    Method_Form -> {CAA, _, _, _, _} = caa(Method_Form, #{}, 0, -1, [0]),
                        io:fwrite("The CAA:~n~p~n", [CAA]),
                        file:write_file("automata.txt", lists:flatten(io_lib:format("~p", [CAA]))),
                        visualisation:graph(CAA)
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

caa({_, _, Method_Name, Arity, Clauses, Method_Term}, MethodState_Map, Last_Transition_State, Pre_assumedState, Transition_States) ->
    clause(Clauses, Method_Name, Arity, length(Clauses) > 1,
        false, maps:merge(MethodState_Map, #{Method_Term => Last_Transition_State}),
        0, [], [], [Last_Transition_State], Pre_assumedState, Transition_States, length(Clauses), -1).

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
clause([], _, _, _, _, _, Max_StateMethod, Clauses_Last_Transition_States, CAA, Parent_Last_Transition_States, _, Transition_States, Length_Clauses, _) ->
    {{lists:max(Parent_Last_Transition_States), CAA}, Clauses_Last_Transition_States, Max_StateMethod, Transition_States, Length_Clauses =/= 0};

   
% when there is only one clause in the method
clause([{_,_,_,_,Clause_Body}], Method_Name, Arity, false, false, MethodState_Map, _, _, _, Parent_Last_Transition_States, Pre_assumedState, Transition_States, Length_Clauses, _) ->
    MAX_CAA_State = lists:max(Transition_States),
    {Delta, Clause_Last_Transition_States, NEW_Max_State, Clause_Transition_States, NoRecursion} = 
        sequential(Clause_Body, Method_Name, Arity, MAX_CAA_State, MethodState_Map, [], Parent_Last_Transition_States, Pre_assumedState, Transition_States),
      
    clause([], Method_Name, Arity, false, false, MethodState_Map, NEW_Max_State, Clause_Last_Transition_States, Delta,
                Parent_Last_Transition_States, Pre_assumedState, Clause_Transition_States, noRecursion(NoRecursion, Length_Clauses), -1);

% when there are many clauses in the method and
% this is the first clause
clause([{_,_,_,_,Clause_Body}|Xs], Method_Name, Arity, true, false, MethodState_Map, _, _, _, Parent_Last_Transition_States, Pre_assumedState, Transition_States, Length_Clauses, _) ->
    MAX_CAA_State = lists:max(Transition_States),
    case Pre_assumedState =/= -1 of
        true -> % i.e. when this is a method call, from inside a recieve block
            {Delta, ClauseLast_Transition_States, _, Clause_Transition_States, NoRecursion} = 
                sequential(Clause_Body, Method_Name, Arity, MAX_CAA_State+2, MethodState_Map, [], [MAX_CAA_State+2],
                    Pre_assumedState, [MAX_CAA_State+2] ++ Transition_States); % MAX_CAA_State+2 coz MAX_CAA_State+1 is going to be our Clauses Pre_assumedState
        _   ->  % otherwise we can pass the "clause ending pre_assumed state" in the place of Pre_assumedState (coz it's -1), so that it will do the 
                % changing of states for us. 
            {Delta, ClauseLast_Transition_States, _, Clause_Transition_States, NoRecursion} = 
                sequential(Clause_Body, Method_Name, Arity, MAX_CAA_State+2, MethodState_Map, [], [MAX_CAA_State+2],
                    MAX_CAA_State+1, [MAX_CAA_State+2] ++ Transition_States) % the first parameter at this line
    end,
    case Delta == [] of
        false ->
            {Changed_Delta, Changed_Transition_States, Changed_Last_Transition_States} =
                clause_changeTracesStates(lists:reverse(Delta), Pre_assumedState, MAX_CAA_State+1, MethodState_Map, Clause_Transition_States),
            % % change the last transition state to the clause pre-assumed state
            % {Changed_Delta, Changed_Last_Transition_States, _, Changed_Transition_States, _} =
            %     changeTracesStates(lists:reverse(Delta), ClauseLast_Transition_States, [], MAX_CAA_State+1, MethodState_Map, ClauseLast_Transition_States, Clause_Transition_States),
            % clause(Xs, Method_Name, Arity, true, true, MethodState_Map, lists:max(Changed_Transition_States),
            %     Changed_Last_Transition_States,
            %     addTraces(Changed_Delta, Parent_Last_Transition_States, MAX_CAA_State+1, undefined),
            %     Parent_Last_Transition_States, Pre_assumedState, Changed_Transition_States, noRecursion(NoRecursion, Length_Clauses), MAX_CAA_State+1);
            clause(Xs, Method_Name, Arity, true, true, MethodState_Map, lists:max(Changed_Transition_States),
                Changed_Last_Transition_States,
                    addTraces(lists:reverse(Changed_Delta), Parent_Last_Transition_States, MAX_CAA_State+1, undefined),
                        Parent_Last_Transition_States, Pre_assumedState, Changed_Transition_States, noRecursion(NoRecursion, Length_Clauses), MAX_CAA_State+1);
        _ -> % when there was just a recursive call inside this clause
            clause(Xs, Method_Name, Arity, true, true, MethodState_Map, MAX_CAA_State+1,
                [],
                addTraces(Delta, Parent_Last_Transition_States, lists:nth(1, Parent_Last_Transition_States)-1, unlabelled),
                Parent_Last_Transition_States, Pre_assumedState, Transition_States, Length_Clauses-1, MAX_CAA_State+1)
end;
% when there are many clauses in the method and
% this is the Nth clause where N > 1
clause([{_,_,_,_,Clause_Body}|Xs], Method_Name, Arity, true, true, MethodState_Map, Max_State_lastClause,
    Clauses_Last_Transition_States, CAA, Parent_Last_Transition_States, Pre_assumedState, Transition_States, Length_Clauses, ClausesPre_assumedState) ->
        case Pre_assumedState =/= -1 of
            true ->
                {Delta, ClauseLast_Transition_States, _, Clause_Transition_States, NoRecursion} = 
                    sequential(Clause_Body, Method_Name, Arity, Max_State_lastClause+1, MethodState_Map, [], [Max_State_lastClause+1],
                        Pre_assumedState, [Max_State_lastClause+1|Transition_States]); % Max_State_lastClause+1 coz of the unlabelled or undefined transition
            _   ->
                {Delta, ClauseLast_Transition_States, _, Clause_Transition_States, NoRecursion} = 
                    sequential(Clause_Body, Method_Name, Arity, Max_State_lastClause+1, MethodState_Map, [], [Max_State_lastClause+1],
                        ClausesPre_assumedState, [Max_State_lastClause+1|Transition_States])
        end,
        case Delta == [] of
            false ->
                {Changed_Delta, Changed_Transition_States, Changed_Last_Transition_States} =
                    clause_changeTracesStates(lists:reverse(Delta), Pre_assumedState, ClausesPre_assumedState, MethodState_Map, Clause_Transition_States),
                % {Changed_Delta, Changed_Last_Transition_States, _, Changed_Transition_States, _} =
                % changeTracesStates(lists:reverse(Delta), ClauseLast_Transition_States, [], ClausesPre_assumedState, MethodState_Map, ClauseLast_Transition_States, Clause_Transition_States),
                % clause(Xs, Method_Name, Arity, true, true, MethodState_Map, lists:max(Changed_Transition_States),
                % Changed_Last_Transition_States ++ Clauses_Last_Transition_States,
                % CAA ++ addTraces(Changed_Delta, Parent_Last_Transition_States, Max_State_lastClause, undefined),
                % Parent_Last_Transition_States, Pre_assumedState, Changed_Transition_States, noRecursion(NoRecursion, Length_Clauses), ClausesPre_assumedState);
                clause(Xs, Method_Name, Arity, true, true, MethodState_Map, lists:max(Changed_Transition_States),
                Changed_Last_Transition_States,
                    CAA ++ addTraces(lists:reverse(Changed_Delta), Parent_Last_Transition_States, Max_State_lastClause, undefined),
                            Parent_Last_Transition_States, Pre_assumedState, Changed_Transition_States, noRecursion(NoRecursion, Length_Clauses), ClausesPre_assumedState);
            _ ->  % when there was just a recursive call inside this clause
                clause(Xs, Method_Name, Arity, true, true, MethodState_Map, Max_State_lastClause,
                Clauses_Last_Transition_States,
                CAA ++ addTraces(Delta, Parent_Last_Transition_States, lists:nth(1, Parent_Last_Transition_States)-1, unlabelled),
                Parent_Last_Transition_States, Pre_assumedState, Transition_States, Length_Clauses-1, ClausesPre_assumedState)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% changes the max state of the clauses to the pre assumed state
% delta, pre-assumed state, MethodState_Map

% The 2 clauses blow cover: when the last expression of this clause is a receive expression, then we don't have to do anything coz 
% we have already dealth with this case by now
clause_changeTracesStates([{First_State, Label, Pre_assumedState}|Xs], Pre_assumedState, _, _, Transition_States) ->
    {[{First_State, Label, Pre_assumedState}|Xs], Transition_States, [Pre_assumedState]};
clause_changeTracesStates([{First_State, Label, ClausesPre_assumedState}|Xs], _, ClausesPre_assumedState, _, Transition_States) ->
    {[{First_State, Label, ClausesPre_assumedState}|Xs], Transition_States, [ClausesPre_assumedState]};

clause_changeTracesStates([{First_State, Label, Last_State}|Xs], _, ClausesPre_assumedState, MethodState_Map, Transition_States) ->
    case lists:member(Last_State, maps:values(MethodState_Map)) of
        % if the last state is not representing a recursion
        false   ->  {[{First_State, Label, ClausesPre_assumedState}|Xs], lists:delete(Last_State, Transition_States), [ClausesPre_assumedState]};
        _       ->  {[{First_State, Label, Last_State}|Xs], Transition_States, [Last_State]}
end.  
%%%%%%%%%%%%%%%%%%%%%%%%

% (Expression, Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States,  Last_Pre_assumedStates, All the transition state ) 
% sequential works on sequential part of the code 

sequential([],_,_,Max_State,_,Delta, Last_Transition_States, _, Transition_States) ->
    % In the tupe the last element represent that there was no recursion in this method
   {lists:reverse(Delta), Last_Transition_States, Max_State, Transition_States, true}; 

% when there is a send in the clause
sequential([{_,_,'!',Process_ID,Data}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, Last_Pre_assumedState, Transition_States) ->
    sequential(Xs, Method_Name, Arity, Max_State+1, MethodState_Map,
        addTraces(Delta, Last_Transition_States, Max_State, {send, Process_ID, Data}),
            [Max_State+1],  Last_Pre_assumedState, [Max_State+1|Transition_States]);

% When there is a recursion i.e. call expression
sequential([{call,_,{_,_,_}, _, Method_Term}|_], _, _, Max_State, MethodState_Map, Delta, Last_Transition_States, _, Transition_States) -> 
    Call_MethodState = maps:get(Method_Term, MethodState_Map), % get the method starting state
    case Delta == [] of
                % when there was no send and receive before
                % and now it encounters recursion
                true -> {[], Last_Transition_States, Max_State, Transition_States, false};
                % when there was send and receive before
                % this recursion call
                _ -> changeTracesStates(Delta, Last_Transition_States, [], Call_MethodState, MethodState_Map, Last_Transition_States, Transition_States)
end;

sequential([{'receive', _, Body}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States,  Last_Pre_assumedState, Transition_States) ->
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
    {Recv_Delta, Recv_Max_State, N_Clauses, Recv_Transition_States} = 
        receive_block(Body, Pre_assumedState, Max_State + 1, Method_Name, Arity, MethodState_Map, Delta, Last_Transition_States, length(Body), Transition_States),
        case N_Clauses == 0 of % when all the clauses have recursion
            true    -> {lists:reverse(Recv_Delta), [Pre_assumedState], Recv_Max_State, [Pre_assumedState|Recv_Transition_States], false};
            _       -> sequential(Xs, Method_Name, Arity, Recv_Max_State, MethodState_Map, Recv_Delta, [Pre_assumedState], Last_Pre_assumedState, [Pre_assumedState|Recv_Transition_States])
end;



sequential([{function, Anno, CallMethod_Name, CallArity, Clauses, Method_Term}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States,  Last_Pre_assumedState, Transition_States) ->
    case caa({function, Anno, CallMethod_Name, CallArity, Clauses, Method_Term}, MethodState_Map, lists:nth(1, Last_Transition_States), Last_Pre_assumedState, Transition_States) of
        {{_, Func_Delta}, Func_Last_Transition_States, Func_Max_State, Func_Recv_Transition_States, true} -> % when there was no recursion inside this child method
            sequential(Xs, Method_Name, Arity, Func_Max_State, MethodState_Map, lists:reverse(Func_Delta) ++ Delta , Func_Last_Transition_States,  Last_Pre_assumedState, Func_Recv_Transition_States);
        {{_, Func_Delta}, Func_Last_Transition_States, Func_Max_State, Func_Recv_Transition_States, false} -> % when there was recursion inside this child method
            {lists:reverse(Delta) ++ Func_Delta, Func_Last_Transition_States, Func_Max_State, Func_Recv_Transition_States, false}
    end;

% when none of the above
sequential([_|Xs], Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, Last_Pre_assumedState, Transition_States) -> 
    sequential(Xs, Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, Last_Pre_assumedState, Transition_States).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% deals with the clauses of a receive block
% ([{'caluse', Anno, [receive], [], [expressions]}|Xs],
% pre-assumedState, the last receive clause max state,
% Method name, Method arity, MethodState_Map, delta, last transition state,
% or the starting state of the receive block, number of clauses)
receive_block([], _, LastClause_Max_State, _, _, _, Delta, _, N_Clauses, Transition_States) ->
    {Delta, LastClause_Max_State, N_Clauses, Transition_States};
receive_block([{_, _, Recv, _, Body}|Xs], Pre_assumedState, LastClause_Max_State, Method_Name, Arity, MethodState_Map, Delta, Last_Transition_States, N_Clauses, Transition_States) ->
    {Recv_Delta, _, Recv_Max_State, Recv_Transition_States, NoRecursion} = 
        sequential(Body, Method_Name, Arity, LastClause_Max_State+1, MethodState_Map,
            addTraces(Delta, Last_Transition_States, LastClause_Max_State, {recv, Recv}),
                [LastClause_Max_State+1], Pre_assumedState, [LastClause_Max_State+1|Transition_States]),
    {Pre_assumedState_Delta, Pre_assumedStateTransition_States }= 
        recv_changeTracesStates(lists:reverse(Recv_Delta), Pre_assumedState, MethodState_Map, Recv_Transition_States),
        receive_block(Xs, Pre_assumedState, Recv_Max_State, Method_Name, Arity, MethodState_Map, Pre_assumedState_Delta, Last_Transition_States, noRecursion(NoRecursion, N_Clauses), Pre_assumedStateTransition_States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% changes the max state of the receive clause to the pre assumed state
% delta, pre-assumed state, MethodState_Map

recv_changeTracesStates([{First_State, Label, Last_State}|Xs], Pre_assumedState, MethodState_Map, Transition_States) ->
    case lists:member(Last_State, maps:values(MethodState_Map)) of
        % if the last state is not representing a recursion
        false   ->  {[{First_State, Label, Pre_assumedState}|Xs], lists:delete(Last_State, Transition_States)};
        _       ->  {[{First_State, Label, Last_State}|Xs], Transition_States}
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

changeTracesStates([], _, Delta, _, _, New_Last_Transition_States, Transition_States) -> 
    {Delta, New_Last_Transition_States, lists:max(New_Last_Transition_States), Transition_States, false};
changeTracesStates([{First_State, Label, Last_State}|Xs], Last_Transition_States, Delta, Method_Call_State, MethodState_Map, Last_Transition_States, Transition_States) ->
    case lists:member(Last_State, Last_Transition_States) andalso lists:member(Last_State, maps:values(MethodState_Map)) == false of
        % when the current trace is one of the last transtions trace and the transition state 
        % of this trace is not representing recursion coz it can inside a receive block
        true -> changeTracesStates(Xs, Last_Transition_States, [{First_State, Label, Method_Call_State}|Delta],
                    Method_Call_State, MethodState_Map, [First_State], lists:delete(Last_State, Transition_States));
        _    ->  changeTracesStates(Xs, Last_Transition_States, [{First_State, Label, Last_State}|Delta],
                    Method_Call_State, MethodState_Map, Last_Transition_States, Transition_States)
end;
changeTracesStates([{First_State, Label, Last_State}|Xs], Last_Transition_States, Delta, Method_Call_State, MethodState_Map, New_Last_Transition_States, Transition_States) ->
    case lists:member(Last_State, Last_Transition_States) andalso lists:member(Last_State, maps:values(MethodState_Map)) == false of
        % when the current trace is one of the last transtions trace and the transition state 
        % of this trace is not representing recursion coz it can inside a receive block
        true -> changeTracesStates(Xs, Last_Transition_States, [{First_State, Label, Method_Call_State}|Delta],
                    Method_Call_State, MethodState_Map, [First_State|New_Last_Transition_States], lists:delete(Last_State, Transition_States));
        _    ->  changeTracesStates(Xs, Last_Transition_States, [{First_State, Label, Last_State}|Delta],
                    Method_Call_State, MethodState_Map, New_Last_Transition_States, Transition_States)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noRecursion(true, Length) -> Length;
noRecursion(false, Length) -> Length-1.
