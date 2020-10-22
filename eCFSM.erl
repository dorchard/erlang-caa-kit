-module(eCFSM).
-export([main/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes the filename (with extension) and method name
% without parentheses and then parse the method
% in that file into a form and return an
% Abstract form of that method.
% If the method containes two methods with similar
% name (overloading) it will return the both for rn.

main(Filename, Method) ->
    case parseToForm(Filename) of
        {error, OpenError} -> OpenError;
        Form -> getMethod(Form, list_to_atom(Method))
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

getMethod([], _) ->
    [];
getMethod([{function, L, Method, A, B}|Xs], Method) ->
    [{function, L, Method, A, B}|getMethod(Xs, Method)]; 
getMethod([_|Xs], Method) ->
    getMethod(Xs, Method).
