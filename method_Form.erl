-module(method_Form).
-export([getMethod/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% takes a form and return the method part 
% Form, Method_Name, Arity, Form, Parents -> list containing all the parents
getMethod([], _, _, _, _) ->
    error;
getMethod([{function, L, Method, NumArgu, Body}|_], Method, NumArgu, Form, Parents) ->
    {function, L, Method, NumArgu, getMethod_Clause(Body, Parents, Form), atom_to_list(Method) ++ "->" ++ integer_to_list(NumArgu)}; 
getMethod([_|Xs], Method, NumArgu, Form, Parents) ->
    getMethod(Xs, Method, NumArgu, Form, Parents).

% {clause, Anno, Aritity, Arrow, Body}
% Parents -> list containing all the parents
% Form -> file form
getMethod_Clause([], _, _) ->
    [];
getMethod_Clause([{_, Anno, Aritity, _, Body}|Xs], Parents, Form) ->
    [{clause, Anno, Aritity, [], lists:reverse(getMethod_CallCheck(Body, Parents, Form, []))}| getMethod_Clause(Xs, Parents, Form)].

% Expressions
% Parents -> list containing all the parents
% Form -> file form
% New_Form -> new form of the method
getMethod_CallCheck([], _, _, New_Form) ->
    New_Form;
% for call expression
getMethod_CallCheck([{call, Anno1, {Var, Anno2, Call_To}, Arity}|Xs], Parents, Form, New_Form) ->
    Call_Method = atom_to_list(Call_To) ++ "->" ++ integer_to_list(length(Arity)),
    case lists:member(Call_Method, Parents) of
        % if it's not a recursive call or a call to one of the parent method then
        false   -> getMethod_CallCheck(Xs, Parents, Form, [getMethod(Form, Call_To, length(Arity), Form, [Call_Method|Parents])|New_Form]);
        _       -> getMethod_CallCheck(Xs, Parents, Form, [{call, Anno1, {Var, Anno2, Call_To}, Arity, Call_Method}|New_Form])
end;
% for receive block
getMethod_CallCheck([{'receive', Anno, Clauses}|Xs], Parents, Form, New_Form) ->
    getMethod_CallCheck(Xs, Parents, Form, [{'receive', Anno, getMethod_ReceiveBlock(Clauses, Parents, Form)}|New_Form]);
getMethod_CallCheck([X|Xs], Parents, Form, New_Form) ->
    getMethod_CallCheck(Xs, Parents, Form, [X|New_Form]).


% receive block clauses, 
% Parents -> list containing all the parents
% Form -> file form
getMethod_ReceiveBlock([], _, _) ->
    [];
getMethod_ReceiveBlock([{_, Anno, Recv, _, Body}|Xs], Parents, Form) ->
    [{clause, Anno, Recv, [], lists:reverse(getMethod_CallCheck(Body, Parents, Form, []))}|getMethod_ReceiveBlock(Xs, Parents, Form)].



