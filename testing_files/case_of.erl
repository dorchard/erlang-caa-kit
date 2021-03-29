-module(case_of).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c(NumMessages) -> 
    receive
      {msg, X} ->
          case X of
              0 -> io:fwrite("I'm out");
              N -> io:fwrite("Server got (~w): ~s~n", [NumMessages, X]),
              c(NumMessages+1)
           end;
      stop -> ok 
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

c1(NumMessages) -> 
    receive
        {msg, X} ->
          case X of
              0 -> io:fwrite("I'm out");
              N -> io:fwrite("Server got (~w): ~s~n", [NumMessages, X]),
              cl4(recv, a, N)
           end;
         stop -> ok 
end.

cl4(recv,S,Z) ->
    receive
        {get, P} -> P!S, cl4(send, S,Z);
        {put, X} -> cl4(recv, X,Z)
end;
cl4(send, S,Z) ->
    cl4(send, S,Z);
cl4(X, S, Z) ->
    X!S,
    Z!S,
    cl4(X, S, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

c2(NumMessages) -> 
    receive
        {msg, X} ->
          case X of
              0 -> io:fwrite("I'm out"), X!hi;
              N -> io:fwrite("Server got (~w): ~s~n", [NumMessages, X]),
              cl4(recv, a, N)
           end;
         stop -> ok 
end.

     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
