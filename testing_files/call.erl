-module(call).
-compile(export_all).

%%%%%%%%%%%%%%%%%%% call expression

call(S, Z) -> 
    S!Z,
    S!4,
    call(S,Z).

%%%%%%%%%%%%%%%%%%%
