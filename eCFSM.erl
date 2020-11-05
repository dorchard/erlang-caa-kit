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
                    X -> convert(X)
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
% this is just a prototype which is only
% works for example 2.2/ mem(S)
% c(eCFSM), rp(eCFSM:main("toRead2.erl", "mem", 1)).
convert({_,_,Name,Arity,[X|_]}) ->
    clause(X, Name, Arity, 0).

%   clause, Arity
clause({_,_,_,_,X}, Name, Arity, State) -> 
    {0, caa(X, Name, Arity, State)}.

caa([{'receive',_,X}|_], Name, Arity, State) -> 
    receiveM(X, Name, Arity, State).

receiveM([], _, _, _) -> 
    [];
receiveM([{_,_,[Rec],_,[{call,_,{_,_,Name}, NumsArgu}]}], Name, Arity, State) when length(NumsArgu) == Arity -> 
    {State,{rec, Rec}, 0};
receiveM([{_,_,[Rec],_,Body}|Xs], Name, Arity, State) -> 
    [{State,{rec, Rec}, State + 1} , receiveM(Xs, Name, Arity, State)] ++ [receiveBody(Body,Name, Arity, State+1)].

receiveBody([{_,_,'!',ID,Data},{call,_,{_,_,Name},NumArgu}|_], Name, Arity, State) when length(NumArgu) == Arity -> 
    {State, {send, ID, Data}, 0}.