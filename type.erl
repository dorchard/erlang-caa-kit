-module (type).
-export_type([state/0, transition/0, label/0, caa/0]).

% state() represents the state which we at
% during a transtion.
-type state() ::    integer()
               |    string().

% Specifies a single transition step (of the delta function)

% transition() represents a tuple containing
% 3 elements, where the 1st element represents
% the start state() of the transtion, the 2nd
% element represents the label() and the 3rd
% element represents the next state after the
% transition.
-type transition() :: {state(), label(), state()}.

% Representing communication actions
-type label() ::

   % represents pid!expr
   {send, erl_parse:abstract_expr(), erl_parse:abstract_expr()}

  % represents ?pat
  | {recv, erl_parse:abstract_expr()}

  % no communication, used to represent branching
  | unlabelled.

% Communicating Actor Automata comprises a start state
% and a list (set) of transitions
-type caa() :: {state(), [transition()]}.
