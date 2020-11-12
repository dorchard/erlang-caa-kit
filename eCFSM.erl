-module(eCFSM).
-export([main/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes the filename (with extension) and method name
% without parentheses and then parse the method
% in that file into a form and return an
% Abstract form of that method.
% If the method containes three methods with similar
% 1) filename , 2) method name and 3) number of 
% parameter that method has to prevent overloading methods

main(Filename, Method, NumArgu) ->
    case parseToForm(Filename) of
        {error, OpenError} -> OpenError;
        Form -> case getMethod(Form, list_to_atom(Method), NumArgu) of
                    error -> "No such method found";
                    X -> X, convert(X)
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

% takes a form and return the method part 

getMethod([], _, _) ->
    error;
getMethod([{function, L, Method, NumArgu, B}|_], Method, NumArgu) ->
    {function, L, Method, NumArgu, B}; 
getMethod([_|Xs], Method, NumArgu) ->
    getMethod(Xs, Method, NumArgu).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this is just a prototype of caa which works for every example 
% except when there is a call to different method and
% a method with multiple pattern-matching
% example are provided in example.erl and toRead2.erl   
% c(eCFSM), rp(eCFSM:main("example.erl", "_", _)).

% {function, ANNO, Method_Name, Arity, [X|_] (method clauses)}
convert({_,_,Name,Arity,[X|_]}) ->
    clause(X, Name, Arity, 0).

%%%%%%%%%%%%%%%%%%%%%%%%

%   {clause, ANNO, Arity, Arrow, X (body of the clause)}, Method_Name, Arity, CAA_State
clause({_,_,_,_,X}, Name, Arity, State) -> 
    {State, caa(for_Recv(lists:reverse(X), []), Name, Arity, State, [])}.

%%%%%%%%%%%%%%%%%%%%%%%%%

% caa works on sequential part of the code 
caa([],_,_,_, Delta) ->
    lists:reverse(Delta);
% when there was no send and receive before
% and now it encounters recursion
caa([{call,_,{_,_,Name}, NumArgu}|_], Name, Arity, _,  []) -> 
    case length(NumArgu) == Arity of
        true -> [];
        _    -> [] % overloading, to do
end;
% when there was send and receive before
% and now recursion
caa([{call,_,{_,_,Name}, NumArgu}|_], Name, Arity, State,  [Y|Ys]) -> 
    case length(NumArgu) == Arity of
        true -> {_, Communication, _ } = Y, 
                caa([],Name, Arity, State, [{State - 1, Communication, 0}|Ys]);
        _    -> [] % overloading, to do
end;
% call to another method
% caa([{call,_,{_,_,OMethod}, _}|_], _, _, _,  [Y|Ys]) -> 
%     [];
% when there is a send in the clause
caa([{_,_,'!',P_ID,Data}|Xs], Name, Arity, State, Delta) -> 
    caa(Xs, Name, Arity, State+1, [{State,{send, P_ID, Data}, State+1}|Delta]);
% when there is a receive in the clause
caa([{'receive',_,X}|_], Name, Arity, State, Delta) -> 
    caa([], Name, Arity, State, receiveM(X, Name, Arity, State, []) ++ Delta);
% when none of the above
caa([_|Xs], Name, Arity, State, Delta) -> 
    caa(Xs, Name, Arity, State, Delta).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% receiveM works on the receive block
receiveM([], _, _, _, Recv_Block) -> 
    lists:reverse(Recv_Block);
% {[clause, ANNO, [Recv (case clause)],
% Arrow, Body (case clause expression)|Xs], Name, Arity,
% State (current State), Recv_Block (The whole receive block)}
receiveM([{_,_,[Recv],_,Body}|Xs], Name, Arity, State, Recv_Block) -> 
    receiveM(Xs, Name, Arity, State, Recv_Block ++ caa(Body, Name, Arity, State+1, [{State, {recv, Recv}, State + 1}])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% put all the stuff after the receive block
% in the receive case clause body
for_Recv([],List) -> 
    List;
% when it find the receive block
for_Recv([{'receive', ANNO, Body}|Xs], List) -> 
    for_Recv(Xs, [{'receive', ANNO, recv_Body(Body, List, [])}]);
for_Recv([X|Xs], List) -> 
    for_Recv(Xs, [X|List]).

% add all the stuff which was after the receive block to
% this receive block clauses body 
recv_Body([], _, Main) ->
    lists:reverse(Main);
recv_Body([{clause, ANNO, Recv, Arrow, Body}|Xs], List, Main) ->
    recv_Body(Xs, List, [{clause, ANNO, Recv, Arrow, for_Recv(lists:reverse(Body), []) ++ List}|Main]).
