-module(eCFSM).
-export([main/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% The Main method which takes the MethodName and FileName as input and do the stuff %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% MethodName and FileName type string
% The mathod name without the paranthesis
% The FileName is the file while contains the Method. Needs to include the extension of the filename
%  e.g. main("client", "toRead.erl")

main(MethodName, FileName) -> Method = getMethod(generateForms(getToken(readFile(FileName)), [], []), MethodName),
                                case Method == MethodName ++ " not found" of 
                                    true -> Method;
                                    _ -> parse(Method)
                                end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get the File And remove all the comments from it %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% thanks https://stackoverflow.com/questions/4905247/erlang-read-from-file-the-first-5-rows

readFile(FileName) -> {ok, IO} = file:open(FileName, read),
                     Data = readFile([], IO),
                     file:close(FileName),
                     unicode:characters_to_list(lists:reverse(Data), unicode).

readFile(Data, FileName) -> case file:read_line(FileName) of
                                {ok, Line} ->  readFile([removeComments(Line)|Data], FileName);
                                _ -> Data 
                            end.


% thanks https://rosettacode.org/wiki/Strip_comments_from_a_string
% https://erlang.org/doc/man/lists.html#takewhile-2

removeComments(String) ->	lists:takewhile(fun not_comment/1, String).
 
not_comment($%) -> false;
not_comment(_) -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get the Token from the Data string %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% thanks https://leafo.net/erlang/Parse_tools_in_Erlang.pdf
% https://erlang.org/doc/man/erl_scan.html#string-1

getToken(Data) -> {ok, Token, _} = erl_scan:string(Data), Token.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Generate Forms from the token %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%http://erlang.org/doc/man/erlang.html#tuple_size-1

 % idea behind this method is that it takes the first token and then traverse until  it find the token in which the tuple is
% {dot, _} and by that we can assure that we have find the form for that particular entity (methods, term, etc)
% and then put those tupes of that form kind inside a new list and put that list inside a big list which
% which will contain all the form lists and then recurse

generateForms([], Forms, _) -> lists:reverse(Forms);
generateForms([X|Xs], Forms, Form) -> case tuple_size(X) == 2 of 
                                        true -> {A, _} = X, case A == dot of 
                                            true -> generateForms(Xs,[lists:reverse([X|Form])|Forms], []);
                                            _ -> generateForms(Xs,Forms, [X|Form])
                                        end;
                                    _ -> generateForms(Xs,Forms, [X|Form]) 
                                    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get the method from the Data containing the source code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% http://erlang.org/doc/man/erlang.html#atom_to_list-1
% http://erlang.org/doc/man/erlang.html#tuple_size-1

getMethod([], MethodName) -> MethodName ++ " not found";
getMethod([X|Xs], MethodName) -> [First|_] = X, 
                                   case tuple_size(First) == 3 of
                                       true -> {_,_, Name} = First,
                                           case atom_to_list(Name) == MethodName of
                                               true ->  X;
                                               _ -> getMethod(Xs, MethodName)
                                           end;
                                       _ -> getMethod(Xs, MethodName)
                                   end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% parse the Token into parse_tree %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% https://erlang.org/doc/man/erl_parse.html#parse_exprs-1
parse(Token) -> case erl_parse:parse_form(Token) of
                {ok, Form} -> Form ;
                {_, E} -> E
            end.
