-module(function).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Function expression i.e. call to another function

f(S, Z) ->
    S!Z,
    f_call(S,Z),
    receive
        x -> S!x;
        y -> S!y
    end,
    S!Z.

f_call(S, Z) ->
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end,
    S!Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f1(S, Z) ->
    S!Z,
    receive
        x -> S!x;
        y -> S!y
    end,
    f_call(S,Z),
    S!Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f2(S, Z) ->
    S!Z,
    f1_call(Z),
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end,
    S!Z.

f1_call(Z) ->
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f3(S, Z) -> 
    f1_call(Z),
    f1_call(Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f4(S, Z) ->
    f1_call(Z),
    f2_call(Z, S).

f2_call(S, Z) -> 
    f1_call(Z),
    f2_call(S, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f5(S, Z) -> 
    f1_call(Z),
    f3_call(S, Z).

f3_call(S, Z) ->
    f1_call(Z),
    f5(S, Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f6(S, Z) ->
    f1_call(Z),
    c(recv, S, Z),
    f1_call(Z).

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

f7(S, Z) ->
    f1_call(Z),
    c1(recv, S, Z), 
    f1_call(Z).

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

f8(S, Z) ->
    f1_call(Z),
    c2(recv, S, Z),
    f1_call(Z).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f9(S, Z) ->
    f1_call(Z),
    c3(recv, S, Z),
    f1_call(Z).

c3(recv,S,Z) ->
    receive
            {get, P} -> P!S;
            {"hello", P} -> P!"hi"
    end;
c3(send, S,Z) ->
    Z!S;
c3(X, S, Z) ->
    X!S,
    Z!S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


f10(S, Z) ->
    f1_call(Z),
    c4(recv, S, Z),
    f1_call(Z).

c4(recv,S,Z) ->
    receive
            {get, P} -> P!S;
            {"hello", P} -> P!"hi"
    end;
c4(send, S,Z) ->
    Z!S;
c4(X, S, Z) ->
    X!S,
    Z!S,
    c4(X, S, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


