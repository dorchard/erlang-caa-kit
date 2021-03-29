-module(examples).
-compile(export_all).

%  example 2.2
e(S) ->
    receive
        {get, P} -> P!S, e(S);
        {put, X} -> e(X)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there are few sends 
% before receive 
 
e1(S,Z) ->
    Z!S,
    Z!S+1,
    e(S),
    receive
        {get, P} -> P!S, e1(S,Z);
        {put, X} -> e1(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  when there are other stuff inbetween send and receive 
e2(S,Z) ->
    X = 1,
    Z!S,
    Y = X + 1,
    Z!S+1,
    receive
        {get, P} -> P!S, e2(S,Z);
        {put, X} -> D = Y+1, e2(D,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  when there is a recursive call before receive 
e3(S,Z) ->
    Z!S,
    Z!S+1,
    e3(S, Z),
    receive
        {get, P} -> P!S, e3(S,Z);
        {put, X} -> e3(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  when there is a recursive call first
e4(S,Z) ->
    e4(S, Z),
    receive
        {get, P} -> P!S, e4(S,Z);
        {put, X} -> e4(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there are another receive
% inside a receive
e5(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} ->
            receive
                X -> P!Z, e5(X,Z)
        end;
        {put, X} -> e5(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there is stuff after 
%  a receive
e6(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S;
        {put, X} -> e6(X,Z)
    end,
    Z!S+2,
    receive
        {get, D} -> D!S;
        W -> W!S    
    end,
    Z!S+6,
    e6(S,Z).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there is just stuff send
e7(S,Z) ->
    Z!S,
    Z!S+1,
    Z!S+6,
    e7(S,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %% bug

% when there are another receive
% inside a receive
e8(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S,
                    receive
                        x -> P!y; %% here 
                        get_id -> P!Z
                    end,
                    Z!S+1,
                    e8(S,Z);

        {put, X} -> e8(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when a methods has many clauses

e9(recv,S,Z) ->
    receive
        {get, P} -> P!S, e9(send, S,Z);
        {put, X} -> e9(recv, X,Z)
end;
e9(send, S,Z) ->
    Z!S;
e9(X, S, Z) ->
    X!S,
    Z!S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e10(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S,
                    Z!S+2,
                    receive
                        {get, D} -> D!S,
                                    Z!S+6,
                                    e10(S,Z);
                            W    -> W!S,
                                    Z!S+6,
                                    e10(S,Z)   
                    end;
        {put, X} -> e10(X,Z),
                    Z!S+2,
                    receive
                        {get, D} -> D!S,
                                    Z!S+6,
                                    e10(S,Z);
                            W    -> W!S,
                                    Z!S+6,
                                    e10(S,Z)   
                    end
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e11(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S,
                    receive
                        x -> P!y,
                            Z!S+1,
                            e11(S,Z);
                        get_id -> P!Z,
                                Z!S+1,
                                e11(S,Z)
                    end;

        {put, X} -> e11(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

e12(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} ->
            receive
                X -> P!Z
        end;
        {put, X} -> e12(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

