-module(example).
-compile(export_all).

%  example 2.2
e(S) ->
    receive
        {get, P} -> P!S, e(S);
        {put, X} -> e(X)
end.

% when there are few sends 
% before receive 
e(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S, e(S,Z);
        {put, X} -> e(X,Z)
end.

%  when there are other stuff inbetween send and receive 
e1(S,Z) ->
    X = 1,
    Z!S,
    Y = X + 1,
    Z!S+1,
    receive
        {get, P} -> P!S, e1(S,Z);
        {put, X} -> D = Y+1, e1(D,Z)
end.

%  when there is a recursive call before receive 
e2(S,Z) ->
    Z!S,
    Z!S+1,
    e2(S, Z),
    receive
        {get, P} -> P!S, e2(S,Z);
        {put, X} -> e2(X,Z)
end.

% when there are another receive
% inside a receive
mul_Recv(S,Z) ->
    Z!S,
    Z!S+1,
%    mem(S, Z),
    receive
        {get, P} ->
            receive
                X -> P!Z, mul_Recv(X,Z)
        end;
        {put, X} -> mul_Recv(X,Z)
end.