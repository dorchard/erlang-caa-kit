-module(clause).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c(recv,S,Z) ->
    receive
        {get, P} -> P!S, Z!S;
        {put, X} -> c(recv, X,Z)
end;
c(send, S,Z) ->
    Z!S;
c(X, S, Z) ->
    X!S,
    Z!S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c1(recv,S,Z) ->
    receive
            {get, P} -> P!S, P!S, c1(send, S,Z);
            {put, X} -> X!S, X!S, c1(recv, X,Z)
    end;
c1(send, S,Z) ->
    Z!S, c1(send, S,Z);
c1(X, S, Z) ->
    X!S,
    Z!S, c1(send, S,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c2(recv,S,Z) ->
    receive
            {get, P} -> P!S, P!S;
            {put, X} -> X!S, X!S
    end;
c2(send, S,Z) ->
    Z!S, c2(send, S,Z);
c2(X, S, Z) ->
    X!S,
    Z!S, c2(send, S,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c3(recv,S,Z) ->
    receive
        {get, P} -> P!S, c3(send, S,Z);
        {put, X} -> c3(recv, X,Z)
end;
c3(send, S,Z) ->
    c3(send, S,Z);
c3(X, S, Z) ->
    X!S,
    Z!S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c4(recv,S,Z) ->
    receive
        {get, P} -> P!S, c4(send, S,Z);
        {put, X} -> c4(recv, X,Z)
end;
c4(send, S,Z) ->
    c4(send, S,Z);
c4(X, S, Z) ->
    X!S,
    Z!S,
    c4(X, S, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


c5(recv,S,Z) ->
    receive
        {get, P} -> P!S, c5(recv, S,Z);
        {put, X} -> c5(recv, X,Z)
end;
c5(send, S,Z) ->
    Z!S;
c5(X, S, Z) ->
    X!S,
    Z!S.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


c6(recv,S,Z) ->
    receive
        {get, P} -> X = 1 + 2;
        {put, X} -> c6(recv, X,Z)
end;
c6(send, S,Z) ->
    X = 1 + 2;
c6(X, S, Z) ->
    X!S,
    Z!S.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%