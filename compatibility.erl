-module(compatibility).
-export([deadlock/1]).

-type caa_models() :: [caa()].
    % List containing CAA models of
    % processes

-type caa() :: generate_model:caa().
    % caa() defined in the caa module

-type delta() :: generate_model:delta().
    % delta() defined in the caa module

-type finite_check() :: integer().
    % finite_check() is used for checking
    % that we are not processes' CAA model
    % in the same pattern again and again;
    % basically for preventing infinite loop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec deadlock(caa_models()) -> boolean().
    % takes a list CAA models and return if they're
    % compatible

deadlock([{_, Delta}, {_, S_Delta}|Xs]) ->
    check(Delta, S_Delta, Xs, length(Xs)+1, length(Xs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec check(delta(), delta(), caa(), finite_check(), finite_check()) ->
        boolean().
    % the first two parameter/deltas goes through compatibilty check,
    % third paramter for containing the rest of the processes for again
    % compatibilty check and the last two parameter for preventing infite loop
    % of checking for compatibilty.


check([], [], [], _, _) ->
    true;

% when all the processes are checked
% in every the patterns and no
% compatibility
check(_, _, _, 0, 0) ->
    false;

% when there was no compatibility
% between the two processes.
% So change the Main process.
check(M_Delta, S_Delta, [{_, N_Delta}|Xs], 0, Outer_Length) ->
    check(N_Delta, S_Delta,
    Xs ++ [{-1, M_Delta}],
    length(Xs) + 2,
    Outer_Length - 1); % to prevent infinte loop.

% two processes have "same" labels.
    % send
check([{To, {send, P_id, Data}, From}|Xs],
    [{S_To, {send, S_P_id, S_Data}, S_From}|S_Xs],
    [{_, Next_Delta}|N_Xs],
    Inner_Length, Outer_Length) ->
        check([{To, {send, P_id, Data}, From}|Xs],
            Next_Delta,
            N_Xs ++ [{-1,
            [{S_To, {send, S_P_id, S_Data}, S_From}|S_Xs]}],
            Inner_Length - 1, % so that we don't encounter this pattern again
                              % becausein the above line we're amending the second paramter
                              % to the third parameter (this is for compatibility check
                              % in different pattern. i.e., "3rd clause of check()" ).
            Outer_Length);
    % receive
check([{To, {recv, Recv}, From}|Xs],
    [{S_To, {recv, S_Recv}, S_From}|S_Xs],
    [{_, Next_Delta}|N_Xs],
    Inner_Length, Outer_Length) ->
        check([{To, {recv, Recv}, From}|Xs],
            Next_Delta,
            N_Xs ++ [{-1,
            [{S_To, {recv, S_Recv}, S_From}|S_Xs]}],
            Inner_Length - 1, Outer_Length).

    % unlabelled

% two processes have corresponding labels
    % send and receive

    % receive and send


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec equal(erl_parse:abstract_expr(), erl_parse:abstract_expr()) -> boolean().
    % checks if two abstract expression are equals or not.

equal(Data_1, Data_2) ->
    erl_prettypr:format(Data_1) == erl_prettypr:format(Data_1).