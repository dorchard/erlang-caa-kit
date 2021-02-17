-module(example).
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
 
e(S,Z) ->
    Z!S,
    Z!S+1,
    e(S),
    receive
        {get, P} -> P!S, e(S,Z);
        {put, X} -> e(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  when there is a recursive call before receive 
e2(S,Z) ->
    Z!S,
    Z!S+1,
    e2(S, Z),
    receive
        {get, P} -> P!S, e2(S,Z);
        {put, X} -> e2(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  when there is a recursive call first
e3(S,Z) ->
    e3(S, Z),
    receive
        {get, P} -> P!S, e3(S,Z);
        {put, X} -> e3(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there are another receive
% inside a receive
e4(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} ->
            receive
                X -> P!Z, e4(X,Z)
        end;
        {put, X} -> e4(X,Z)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there is stuff after 
%  a receive
e5(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S;
        {put, X} -> e5(X,Z)
    end,
    Z!S+2,
    receive
        {get, D} -> D!S;
        W -> W!S    
    end,
    Z!S+6,
    e5(S,Z).

% e5(S,Z) ->
%         Z!S,
%     Z!S+1,
%     receive
%         {get, P} -> P!S,
%                     Z!S+2,
%                     receive
%                         {get, D} -> D!S,
%                                     Z!S+6,
%                                     e5(S,Z);
%                             W    -> W!S,
%                                     Z!S+6,
%                                     e5(S,Z)   
%                     end;
%         {put, X} -> e5(X,Z),
%                     Z!S+2,
%                     receive
%                         {get, D} -> D!S,
%                                     Z!S+6,
%                                     e5(S,Z);
%                             W    -> W!S,
%                                     Z!S+6,
%                                     e5(S,Z)   
%                     end
% end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there is just stuff send
e6(S,Z) ->
    Z!S,
    Z!S+1,
    Z!S+6,
    e6(S,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when there are another receive
% inside a receive
e7(S,Z) ->
    Z!S,
    Z!S+1,
    receive
        {get, P} -> P!S,
                    receive
                        x -> P!y;
                        get_id -> P!Z
                    end,
                    Z!S+1,
                    e7(S,Z);

        {put, X} -> e7(X,Z)
end.

% e7(S,Z) ->
%     Z!S,
%     Z!S+1,
%     receive
%         {get, P} -> P!S,
%                     receive
%                         x -> P!y,
%                             Z!S+1,
%                             e7(S,Z);
%                         get_id -> P!Z,
%                                 Z!S+1,
%                                 e7(S,Z)
%                     end;

%         {put, X} -> e7(X,Z)
% end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when a methods has many clauses

e8(recv,S,Z) ->
    receive
        {get, P} -> P!S, e8(send, S,Z);
        {put, X} -> e8(recv, X,Z)
end;
e8(send, S,Z) ->
    Z!S;
e8(X, S, Z) ->
    X!S,
    Z!S.


%%%%%%%%%%%%%%%%%%% For call expressions

call1(S, Z) -> 
    S!Z,
    S!4,
    call1(S,Z).


%%%%%%%%%%%%%%%%%%%%% For Receive expression
recv(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end.

recv1(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end,
    recv1(S, Z).

recv2(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end,
    S!"Hi",
    recv2(S, Z).

recv3(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn",
                    recv3(S,Z)
    end,
    S!"Hi",
    recv3(S, Z).

recv4(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn",
                    recv4(S,Z)
    end,
    S!"Hi",
    S!"Bye",
    recv4(S, Z).

recv5(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end,
    S!"Hi",
    S!"Bye",
    recv5(S, Z).

recv6(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi",
                        receive
                            x -> C!x;
                            y  -> C!y
                        end,
                        C!done;
        {bye, C}  -> C!"Gn",
                    receive
                        x -> C!x;
                        y  -> C!y
                    end
    end,
    S!"Hi",
    S!"Bye",
    recv6(S, Z).

recv7(S, Z) ->
    S!Z,
    receive
        {hello, C} -> receive
                            x -> C!x;
                            y  -> C!y
                        end,
                        C!done;
        {bye, C}  -> receive
                        x -> C!x;
                        y  -> C!y
                    end
    end,
    S!"Hi",
    S!"Bye",
    recv7(S, Z).

recv8(S, Z) ->
    S!Z,
    receive
        x -> S!x;
        y -> S!y
    end,
    receive
        x -> S!x;
        y -> S!y
    end,
    S!"Hi",
    S!"Bye",
    recv8(S, Z).

recv9(S, Z) ->
        S!Z,
    receive
        {hello, C} -> receive
                            x -> C!x;
                            y  -> C!y
                        end,
                        receive
                            x -> C!x;
                            y  -> C!y
                        end,
                        C!done;
        {bye, C}  -> receive
                        x -> C!x;
                        y  -> C!y
                    end,
                    receive
                        x -> C!x;
                        y  -> C!y
                    end
    end,
    % recv9(S, Z). % testing it works
    S!"Hi",
    S!"Bye",
    recv9(S, Z).

recv10(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi",
                        recv10(S,Z);
        {bye, C}  -> C!"Gn",
                    recv10(S,Z)
    end,
    S!"Hi",
    S!"Bye",
    recv10(S, Z).

recv11(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi",
                        receive
                            x -> C!x, 
                                recv11(S, Z);
                            y  -> C!y,
                                recv11(S, Z)
                        end,
                        C!done;
        {bye, C}  -> C!"Gn",
                    receive
                        x -> C!x;
                        y  -> C!y
                    end
    end,
    S!"Hi",
    S!"Bye",
    recv11(S, Z).

recv12(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi",
                        receive
                            x -> C!x, 
                                recv12(S, Z);
                            y  -> C!y,
                                recv12(S, Z)
                        end,
                        C!done;
        {bye, C}  -> C!"Gn",
        receive
            x -> C!x, 
                recv12(S, Z);
            y  -> C!y,
                recv12(S, Z)
        end
    end,
    S!"Hi",
    S!"Bye",
    recv12(S, Z).

%%%%%%%%%%%%%%%%%%%%% For Function expression i.e. call to another function

func(S, Z) ->
    S!Z, 
    func1(S,Z),
    receive
        x -> S!x;
        y -> S!y
    end,
    S!Z.

func1(S, Z) ->
    receive
        {get, P} -> P!Z;
        {"hi", P} -> P!"hello"
    end,
    S!Z.

