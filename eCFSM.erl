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
                    X -> X
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
