-module(eCFSM).
-export([main/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes the filename (with extension), method name and
% its arity. Returns its CAA form.

main(Filename, Method, NumArgu) ->
    case parseToForm(Filename) of
        {error, OpenError} -> OpenError;
        Form -> case method_Form:getMethod(Form, list_to_atom(Method), NumArgu, Form, [Method ++ "->" ++ integer_to_list(NumArgu)]) of
                    error -> "No such method found";
                    {function, _, Method_Name, Arity, X, Method_Term} -> 
                        convert(X, Method_Name, Arity, length(X) > 1,
                                false, 0, #{Method_Term => 0},
                                0, Form)
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
% this is just a prototype of caa which works for every example 
% except when there is a call to different method

% example are provided in example.erl and toRead2.erl   
% c(eCFSM), rp(eCFSM:main("example.erl", "Methodname", Arity(int))).

% ({clause, ANNO, Arity, Arrow, X (body of the clause)},
% Method_Name, Arity, does this method have many clauses,
% we are at Nth clause of this method where N > 1,
% Maximum state, map containing Method_Term and it's states)
convert([], _, _, _, _, _, _, _, _) ->
    [];
% when there is only one clause in the method
convert([{_,_,_,_,X}], Method_Name, Arity, false, false, CAA_State, MethodState_Map, _, File_Form) ->
    {Delta, _, _, _} = caa(X, Method_Name, Arity, CAA_State, MethodState_Map, [], [CAA_State], File_Form),
    {CAA_State, Delta};
% when there are many clauses in the method and
% this is the first one
convert([{_,_,_,_,X}|Xs], Method_Name, Arity, true, false, CAA_State, MethodState_Map, _, File_Form) ->
    {Delta, _, Max_StateClause, _} = caa(X, Method_Name, Arity, CAA_State+1, MethodState_Map, [], [CAA_State+1], File_Form),
    case Delta == [] of
        false ->
            {CAA_State, [{CAA_State, undefined, CAA_State+1}] ++ Delta ++
            convert(Xs, Method_Name, Arity, true, true, CAA_State, MethodState_Map, Max_StateClause, File_Form)};
        _ -> % when there was just a recursive call inside this clause
            {CAA_State, [{CAA_State, unlabelled, CAA_State}] ++ Delta ++
            convert(Xs, Method_Name, Arity, true, true, CAA_State, MethodState_Map, CAA_State, File_Form)}
    end;
% when there are many clauses in the method and
% this is the Nth clause where N > 1
convert([{_,_,_,_,X}|Xs], Method_Name, Arity, true, true, CAA_State, MethodState_Map, Max_StateClause, File_Form) ->
    {Delta, _, New_Max_StateClause, _} = caa(X, Method_Name, Arity, Max_StateClause+1, MethodState_Map, [], [Max_StateClause+1], File_Form),
    case Delta == [] of
        false ->
            [{CAA_State, undefined, Max_StateClause+1}] ++ Delta ++
            convert(Xs, Method_Name, Arity, true, true,  CAA_State, MethodState_Map, New_Max_StateClause, File_Form);
        _ ->  % when there was just a recursive call inside this clause
            [{CAA_State, unlabelled, CAA_State}] ++ Delta ++
            convert(Xs, Method_Name, Arity, true, true,  CAA_State, MethodState_Map, Max_StateClause, File_Form)
end.

%%%%%%%%%%%%%%%%%%%%%%%%

% (Expression, Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States) 
% caa works on sequential part of the code 

caa([],_,_,Max_State,_,Delta, Last_Transition_States, _) ->
    % In the tupe the last element represent that there was no recursion in this method
   {lists:reverse(Delta), Last_Transition_States, Max_State, true}; 

% when there is a send in the clause
caa([{_,_,'!',Process_ID,Data}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, File_Form) ->
    caa(Xs, Method_Name, Arity, Max_State+1, MethodState_Map,
        addTraces(Delta, Last_Transition_States, Max_State, {send, Process_ID, Data}),
        [Max_State+1], File_Form);

% When there is a call to a method
caa([{call,_,{_,_,Method}, NumArgu}|Xs], Method_Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, File_Form) -> 
    MethodCallTo = atom_to_list(Method) ++ "->" ++ integer_to_list(length(NumArgu)),
    case maps:find(MethodCallTo, MethodState_Map) of % when its's a recursion or call to a predecessor method
        {ok, Method_Call_State} -> 
            case Delta == [] of
                % when there was no send and receive before
                % and now it encounters recursion
                true -> {[], Last_Transition_States, Max_State, false};
                % when there was send and receive before
                % this recursion call
                _ -> changeTracesStates(Delta, Last_Transition_States, [], Method_Call_State, MethodState_Map, [])
        end;
        _   -> % when it's a new call to the method
            [] 
end;

% when none of the above
caa([_|Xs], Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, File_Form) -> 
    caa(Xs, Name, Arity, Max_State, MethodState_Map, Delta, Last_Transition_States, File_Form).

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
