-module (type).



% Specifies a single transition step (of the delta function)
-type transition() :: {integer(), action(), integer()}.

% Representing communication actions
-type action() ::

   % represents pid!expr
   {send, erl_parse:abstract_expr(), erl_parse:abstract_expr()}

  % represents ?pat
  | {recv, erl_parse:abstract_expr()}

  % no communication, used to represent branching
  | unlabelled.

% Communicating Actor Automata comprises a start state and a list (set) of transitions
-type caa() :: {integer(), [transition()]}.
