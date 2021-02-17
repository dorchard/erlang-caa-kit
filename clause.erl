-module(clause).
-xompile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c(recv,S,Z) ->
    receive
        {get, P} -> P!S, c(send, S,Z);
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


