-module(recv).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%% Receive expression %%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%

recv(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end.

%%%%%%%%%%%%%%%%%%%%%

recv1(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end,
    recv1(S, Z).

%%%%%%%%%%%%%%%%%%%%%

recv2(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end,
    S!"Hi",
    recv2(S, Z).

%%%%%%%%%%%%%%%%%%%%%

recv3(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn",
                    recv3(S,Z)
    end,
    S!"Hi",
    recv3(S, Z).

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

recv5(S, Z) ->
    S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn"
    end,
    S!"Hi",
    S!"Bye",
    recv5(S, Z).

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

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
    %  recv9(S, Z), % testing it works
    S!"Hi",
    S!"Bye",
    recv9(S, Z).

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%

recv13(S, Z) ->
        S!Z,
    receive
        {hello, C} -> C!"hi";
        {bye, C}  -> C!"Gn",
                     C!"sleep",
                    recv13(S, Z)
    end,
    recv13(S, Z),
    S!"Hi",
    S!"Bye",
    recv13(S, Z).
 
%%%%%%%%%%%%%%%%%%%%%
   
recv14(S, Z) ->
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
    %  recv9(S, Z), % testing it works
    S!"Hi",
    S!"Bye",
    recv14(S, Z).

%%%%%%%%%%%%%%%%%%%%%