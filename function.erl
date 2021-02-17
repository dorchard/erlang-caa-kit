-module(function).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% For Function expression i.e. call to another function
f(S, Z) ->
    S!Z,
    f1(S,Z),
    receive
        x -> S!x;
        y -> S!y
    end,
    S!Z.

f1(S, Z) ->
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end,
    S!Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f2(S, Z) ->
    S!Z,
    receive
        x -> S!x;
        y -> S!y
    end,
    f1(S,Z),
    S!Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f3(S, Z) ->
    S!Z,
    f4(Z),
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end,
    S!Z.

f4(Z) ->
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f5(S, Z) -> 
    f4(Z),
    f4(Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f6(S, Z) ->
    f4(Z),
    f7(Z, S).

f7(S, Z) -> 
    f4(Z),
    f7(S, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f8(S, Z) -> 
    f4(Z),
    f9(S, Z).

f9(S, Z) ->
    f4(Z),
    f8(S, Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f10(S, Z) ->
    f4(Z),
    c(recv, S, Z),
    f4(Z).

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

f11(S, Z) ->
    f4(Z),
    c1(recv, S, Z),
    f4(Z).

c1(recv,S,Z) ->
    receive
            {get, P} -> P!S;
            {put, X} -> X!S
    end;
c1(send, S,Z) ->
    Z!S, c1(send, S,Z);
c1(X, S, Z) ->
    X!S,
    Z!S, c1(send, S,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f12(S, Z) ->
    f4(Z),
    c2(recv, S, Z),
    f4(Z).

c2(recv,S,Z) ->
    receive
            {get, P} -> P!S, c2(recv,S,Z);
            {put, X} -> c2(recv,S,Z)
    end;
c2(send, S,Z) ->
    Z!S, c2(send, S,Z);
c2(X, S, Z) ->
    X!S,
    Z!S, c2(send, S,Z).

