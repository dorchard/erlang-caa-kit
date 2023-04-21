-module(method_Form).
-export([getMethod/5]).

-type form() :: erl_parse:abstract_form().
    % Abstract form of an Erlang form/file.

-type clause() :: [erl_parse:abstract_clause()].
    % Abstract form of an Erlang clause.

-type expressions() :: [erl_parse:abstract_expr()].
    % Abstract form of an Erlang expression.

-type method() :: atom().
    % Method name


-type parents() :: [methodTerm()].
    % list containing methodTerm of current method
    % and of parent methods (if any exist).
    % By a parent method we mean:
    % "lets at the starting we were generating the caa_methodForm
    %  of method 'f()' and somewhere inside 'f()' it calls
    %  method 'a()', then when we are genetrating the caa_methodForm
    %  for a() (in nested sense of 'f()') then a()'s parent method
    %  would be f() and it's parents() -> [method term of a(),  method term of f()]"
    % And it's because it will help in detecting recursion (line 99).

-type methodTerm() :: string().
    % "method() ++ "->" ++ arity()"
    % where method() and arity()
    % are of type string().

-type caa_methodForm() :: caa_methodForm.
    % The caa_methodForm is very similar to
    % form() function declaration, it just
    % have few tweaks:
    % 1) caa_methodForm clauses are of type
    %   caa_clauseForm().
    % 2) At the end of every method's caa_methodForm
    %   there will be the methodTerm() of that
    %   particular method.

-type caa_clauseForm() :: []
                        |   [caa_clauseForm].
    % The caa_clauseForm is very to similar to
    % clause(), but caa_clauseForm body is of
    % type caa_clauseBody()

-type caa_clauseBody() ::   []
                        |   [caa_clauseBody].
    % caa_clauseBody is a list which can contains
    % 'erl_parse:abstract_expr()' and
    % 'form() function declaration (of method*)
    % instead of a
    % "call" erl_parse:abstract_expr() expression
    % when a new method* is getting called'

-type caa_RecvClauseForm() ::   []
                            |   [caa_RecvClauseForm].
    % caa_RecvClauseForm is similar to caa_clauseForm
    % but caa_RecvClauseForm is is case clause of a
    % receive expression where caa_clauseForm is a
    % function clause.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec getMethod(form(), method(), arity(), form(), parents()) ->  error
                                                                |   caa_methodForm().

getMethod([], _, _, _, _) ->
    error;
% when we found the given method's
% erl_parse:abstract_form() function declaration.

%%%%% end of clause %%%%%


getMethod([{function, L, Method, NumArgu, Body}|_], Method, NumArgu, Form, Parents) ->
    {function, L, Method, NumArgu, getMethod_Clause(Body, Parents, Form), atom_to_list(Method) ++ "->" ++ integer_to_list(NumArgu)};

%%%%% end of clause %%%%%


getMethod([_|Xs], Method, NumArgu, Form, Parents) ->
    getMethod(Xs, Method, NumArgu, Form, Parents).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec getMethod_Clause(clause(), parents(), form()) -> caa_clauseForm().


getMethod_Clause([], _, _) ->
    [];

%%%%% end of clause %%%%%


getMethod_Clause([{_, Anno, Aritity, _, Body}|Xs], Parents, Form) ->
    [{clause, Anno, Aritity, [], lists:reverse(getMethod_CallCheck(Body, Parents, Form, []))}| getMethod_Clause(Xs, Parents, Form)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec getMethod_CallCheck(expressions(), parents(), form(), caa_clauseBody()) -> caa_clauseBody().

getMethod_CallCheck([], _, _, New_Body) ->
    New_Body;


%%%%% end of clause %%%%%


% for call expression
getMethod_CallCheck([{call, Anno1, {Var, Anno2, Call_To}, Arity}|Xs], Parents, Form, New_Body) ->
    Call_Method = atom_to_list(Call_To) ++ "->" ++ integer_to_list(length(Arity)),
    case lists:member(Call_Method, Parents) of
        % if it's not a recursive call (or not a call to
        % one of the parent's method) then
        % add the form() function declaration of this new
        % method instead of that call expression to New_Body
        false   -> getMethod_CallCheck(Xs, Parents, Form, [getMethod(Form, Call_To, length(Arity), Form, [Call_Method|Parents])|New_Body]);
        % otherwise just add the call expression to New_Body
        _       -> getMethod_CallCheck(Xs, Parents, Form, [{call, Anno1, {Var, Anno2, Call_To}, Arity, Call_Method}|New_Body])
end;


%%%%% end of clause %%%%%


% for receive expression
getMethod_CallCheck([{'receive', Anno, Clauses}|Xs], Parents, Form, New_Body) ->
    getMethod_CallCheck(Xs, Parents, Form,
       [{'receive', Anno, getMethod_ReceiveBlock(Clauses, Parents, Form)}|New_Body]);


%%%%% end of clause %%%%%


% case expression
getMethod_CallCheck([{'case', Anno, Of, Clauses}|Xs], Parents, Form, New_Body) ->
    getMethod_CallCheck(Xs, Parents, Form, [{'case', Anno, Of, getMethod_ReceiveBlock(Clauses, Parents, Form)}|New_Body]);


%%%%% end of clause %%%%%


% expression that we are not focusing on
getMethod_CallCheck([_|Xs], Parents, Form, New_Body) ->
    % we are not adding the expression to caa_clauseBody(), which for the current version 
    % of CAA model does not contribute in transitions
    getMethod_CallCheck(Xs, % expressions()
      Parents, % parents()
      Form, % form()
      New_Body). % caa_clauseBody()


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec getMethod_ReceiveBlock(clause(), parents(), form()) -> caa_RecvClauseForm().


getMethod_ReceiveBlock([], _, _) ->
    [];

%%%%% end of clause %%%%%


getMethod_ReceiveBlock([{_, Anno, Recv, _, Body}|Xs], Parents, Form) ->
    [{clause, Anno, Recv, [], lists:reverse(getMethod_CallCheck(Body, Parents, Form, []))}|getMethod_ReceiveBlock(Xs, Parents, Form)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%