-module(caa).
-export([main/3]).

-type func() :: string().
    % func() is the function name, for
    % which the user wants to extract the CAA 
    % model. func() is represented as:
    % "<function_name>/<function_Arity>"
    % e.g., 
    % foo(A, B) will be "foo/2"

-type file() :: string().
    % file() is the name of the file which
    % contains the function().

-type location() :: string().
    % location where the user wants the CAA.pdf,
    % graph.dot and automata.txt files to be 
    % created.

-type error() :: {error, parse_file:openError()} 
               | {error, parse_file:errorInfo()}
               | {error, parse_file:line()}
               | string().
    % error() represent errors encounter either while 
    % parsing the file of given file() to form() or
    % while finding the given func() inside the form().

-type caa() :: {start_State(), delta()}.
    % Communicating Actor Automata specifies
    % a tuple representation of Actor-based
    % model erlang code. The tuple contains
    % a staring state and it's delta.

-type start_State() :: integer().
    % start_State() represents the initial
    % state of the caa().

-type delta() :: [transition()].
    % delat() contains list of transitions
    % which represents the transition 
    % relations.

-type transition() :: {state(), label(), state()}.
    % transition represents a tuple containing
    % 3 elements, where the 1st element represents 
    % the start state() of the transtion, the 2nd 
    % element represents the label() and the 3rd 
    % element represents the next state after the
    % transition.

-type state() ::    integer()
               |    string().
    % state() represents the state which we at
    % during a transtion.

-type label() ::    {send, erl_parse:abstract_expr(), erl_parse:abstract_expr()}
                |   {recv, erl_parse:abstract_expr()}
                |   unlabelled.
    % label() specifies the communication/label
    % over which the transition is happening.

-type form() :: erl_parse:abstract_form().
    % form() is the erl_parse:abstract_form() 
    % of the file of given file().

-type method_Form() :: method_Form:caa_methodForm().

-type methodState_Map() :: map().
    % methodState_Map() is a map, where the key is 
    %   method_Form:methodTerm()
    % and the value is the stating state of this given
    % method_Form:methodTerm() method in the delta().
    %
    % It is used for dectecting recursion.

-type last_Transition_State() :: state().
    % last_Transition_State() represents the transtion
    % state in the last taransition of delta().

-type pre_assumedState() :: state().
    % pre_assumedState() represents the pre_assumed next state after a
    % receive or case or many clauses.

-type transition_States() :: [state()].
    % transition_States() is list containing all the transition states
    % happened in the delta() so far.

-type max_StateMethod() :: state().
    % max_StateMethod() represents the max transitino state in the delta().

-type caa_Info() :: {caa(), last_Transition_State(), max_StateMethod(), transition_States(),
                     boolean()}.
    % caa_data() is a tuple containing information about the caa().
    % The boolean() inside the tuple represent if there was "no recursion". 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec main(file(), func(), location()) -> error() | caa().
    % main/3 takes file(), function() and location()
    % as inputs and either return an error if the given 
    % file() or function() do not exist or return a CAA
    % model, also creates automata.txt, graph.dot and 
    % CAA.pdf in the given location() and opens the CAA.pdf.
    %
    % NOTE: automata.txt contains the CAA model, graph.dot 
    % contains the CAA model in dot (graphviz) representation
    % and CAA.pdf represents the visual graphical representation 
    % of the CAA model.


main(Filename, Function, Location) ->
    [Method, Arity] = string:split(Function, "/"),
    % gets the file of given file()
    % and parse it to form()
    case parseToForm(Filename) of 
     % if an error occured during parsing
     {error, Error} -> Error;
     % -type Form :: form().
     Form ->
         % gets the method_Form() from the form() 
         case method_Form:getMethod(Form, % form()
                list_to_atom(Method), % method_Form:method()
                list_to_integer(Arity), % arity()
                Form, [Method ++ "->" ++ Arity]) of % form(), method_Form:parents()

                  % if the given func() not found inside the form()
                  error -> "No such method found";
                  
                  % -type  Method_Form :: method_Form:()
                  Method_Form -> 
                    % extracts the CAA model from the method_Form().
                    % -type CAA :: caa()
                    {CAA,  _, _, _} = caa(Method_Form, % method_Form()
                                        #{}, % #{} is methodState_Map()
                                        0, % last_Transition_State()
                                        -1, %  pre_assumedState()
                                        [0]), % transition_States()
                    
                    % creates the automata.txt file in the given location()
                    Automata = lists:flatten(io_lib:format("~sautomata.txt", [Location])),
                    file:write_file(Automata, lists:flatten(io_lib:format("~p", [CAA]))),
                    % creates the visual representation of the CAA model
                    visualisation:graph(CAA, Location),
                     % standard ouputs the CAA model
                    CAA
             end
end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parseToForm(file()) -> error() | form().
    % parseToForm/1 takes the filename and return 
    % error() or form() according to if there was any
    % error while parsing the file of given filename.

parseToForm(Filename) ->  
    case epp:parse_file(Filename, Filename) of
     % if there was any error       
     {error, OpenError} -> {error, {error, OpenError}};
     {ok, {error, ErrorInfo}} -> {error, {{error, ErrorInfo}}};
     {ok, {eof, Line}} -> {error, {eof, Line}};

     % parsing was succesful
     {ok, Form} -> Form
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec caa(method_Form(), methodState_Map(), last_Transition_State(), pre_assumedState(), 
          transition_States()) -> caa_Info().
      % caa/5 takes the method-form of the given file, map of type methodState_Map(), the 
      % last transition state so far, the pre-assumed state and list of transition states;
      % and return a tuple which contain the CAA model, last transition state of the CAA
      % model, list of all the transition states so far, boolean telling "no recursion" or 
      % not. 

caa({_, _, _, _, Clauses, Method_Term}, MethodState_Map, Last_Transition_State, Pre_assumedState,
  Transition_States) ->
    clause(Clauses, % clauses inside the method_form(): clauses()
      length(Clauses) > 1, % many_clauses()
      false, % false because we are going to start from the very 1st clause: is_Nth_clause()
      maps:merge(MethodState_Map, #{Method_Term => Last_Transition_State}),% methodState_Map()
      0, % lastClause_max_State()
      -1, % becase we're going to start from the very 1st clause: lastClause_Last_Transition_State()
      [], % caa() 
      Last_Transition_State, % the last transition so far: parent_Last_Transition_State()
      Pre_assumedState, % pre_assumedState() of the parent method because this can a be child method
      Transition_States, % transition_States()
      length(Clauses), % to check later if all the clauses had recusrion: length_Clauses()
      -1). % -1 because there is no pre-assumed state for clauses at this moment, because we
           % don't know if there are many clauses or not : pre_assumedState() for this method 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type clauses() :: method_Form:caa_clauseForm().
    % clauses in AST form generated by method_Form:getMethod()

-type many_clauses() :: boolean().
    % many_clauses() is for to check if there are more than 
    % one clause inside a method, when many clauses brings
    % unlabelled transitions

-type is_Nth_clause() :: boolean().
    % is_Nth_clause() is for to know while traversing over the 
    % clauses of a method we are at clause N (where N > 1). It main
    % application is in distinguishing the first clause from the rest
    % when there are multiple clauses inside a method

-type lastClause_max_State() :: state().
   % lastClause_max_State() tells the max transition state of the last
   % clause when there are multiple clauses.

-type lastClause_Last_Transition_State() :: state().
   % similar to last_Transition_State(), but lastClause_Last_Transition_State()
   % tells the last transition state of the last clause when there multiple 
   % clauses inside a method

-type parent_Last_Transition_State() :: state().
   % similar to last_Transition_State(), but parent_Last_Transition_State() tells
   % the last transition state of the parent method as this method (child-method)
   % can be a call from inside of another method (parent-method)

-type length_Clauses() :: integer().
   % length_Clauses() tells the number of clauses a method have. It's main
   % application is in figuring out if all the clauses inside a method had recursion.

-type clausesPre_assumedState() :: state().
   % similar to pre_assumedState(), but clausesPre_assumedState() tells the 
   % end state of all the clauses a method have if there are multiple clauses
   % inside that method.



-spec clause(clauses(), many_clauses(), is_Nth_clause(), methodState_Map(), lastClause_max_State(),
        lastClause_Last_Transition_State(), delta(), parent_Last_Transition_State(), 
        pre_assumedState(), transition_States(), length_Clauses(), clausesPre_assumedState()) ->
            caa_Info().
    % clause/13 deals with the clauses of a method i.e, traversing via the clauses 
    % and assigning them their start state and replacing the last transition state 
    % (if no recursion) of a clause delta with clausesPre_assumedState(). And it passes 
    % the clause body of a particular clause to the expressions/6 which deals with the 
    % delta of a clause. After traversing via all the clauses of a method, clause/13
    % return a tuple which contains the CAA model of that method and some addition 
    % information regarding the CAA model.


% After traversing via all the clauses,
% all the clauses had recursion. i.e.,
% length_Clauses() == 0  (11th parameter of this method)
clause([], _, _, _, _, _, Delta, Parent_Last_Transition_State, _, Transition_States, 0, _) ->
    {{Parent_Last_Transition_State, Delta}, % the CAA model
     -1, % -1 because no point of clause-last transition state
     Transition_States,
     false}; % false because of there was recursion, remember this element represent "no-recursion"


%%%%% end of clause %%%%%


% After traversing via all the clauses,
% not all the clauses or no clauses had recursion in them
clause([], _, _, _, _, LastClause_Last_Transition_State, Delta, Parent_Last_Transition_State, _,
  Transition_States, _, _) ->
    % we pass the LastClause_Last_Transition_State as the last transition state because 
    % all the clauses which didn't had recursion had this state as their last state         
    {{Parent_Last_Transition_State, Delta},
     LastClause_Last_Transition_State, % can be changed with Clause_Transition_States
     Transition_States,
     true};


%%%%% end of clause %%%%%

         
% when there is only one clause in the method
clause([{_,_,_,_,Clause_Body}], false, false, MethodState_Map, _, _, _, 
  Parent_Last_Transition_State, Pre_assumedState, Transition_States, Length_Clauses, _) ->
    % passes the Clause_Body to expressions/6 and gets
    % this clause Delat with other important information about
    % the delta
    {Delta, Clause_Last_Transition_State, Clause_Transition_States, NoRecursion} = 
        expressions(Clause_Body, % expressions()
         MethodState_Map, % methodState_Map()
         [], % delta()
         Parent_Last_Transition_State, % last_Transition_State()
         Pre_assumedState, % pre_assumedState()
         Transition_States), % transition_States()

    % go to the base case with the delta
    % Here -1 represent the paramaters we don't
    % care about in the base cases.
    % Note:  noRecursion/2 reduce length_Clauses() by 1 if there was 
    % recurion inside this clause
    clause([], % clauses(), [] because there was only one clause in this method
     -1, -1, %  many_clauses() and is_Nth_clause()
     MethodState_Map, % methodState_Map()
     -1, % lastClause_max_State()
     Clause_Last_Transition_State, Delta, % lastClause_Last_Transition_State() and delta(),
     Parent_Last_Transition_State, % parent_Last_Transition_State()
     Pre_assumedState, % pre_assumedState()
     Clause_Transition_States, %  transition_States()
     noRecursion(NoRecursion, Length_Clauses), % length_Clauses(),
     Clause_Last_Transition_State); % Clause_Last_Transition_State as clausesPre_assumedState()
                                    % because this is the last transition state and there is no
                                    % clause left as there was only 1 clause in this method


%%%%% end of clause %%%%%
 

% when there are many clauses in this method and
% we are at the very first clause i.e,
% many_clauses() == true and 
% is_Nth_clause() == false (where N > 1)
%  (parameter 2 and 3 respectively)
clause([{_,_,_,_,Clause_Body}|Xs], true, false, MethodState_Map, _, _, _, 
  Parent_Last_Transition_State, Pre_assumedState, Transition_States, Length_Clauses, _) ->
    % gets the so far Max transition/CAA state as this method can be a child-method 
    % i.e., call from a parent method  
    MAX_CAA_State = lists:max(Transition_States),
    % setting pre-assumed state for clauses
    case Pre_assumedState =/= -1 of
      % i.e. when this is a method call, from inside a recieve block
      true -> 
        Clause_Last_Transition_State = Pre_assumedState,
          
        % delta_info()
        % get the delta for this clause with some addition information about the delta
        {Delta, _, Clause_Transition_States, NoRecursion} = 
          % we are passing "MAX_CAA_State+2" in place of last_Transition_State() because 
          % "MAX_CAA_State+2" is the unlabelled transition for this clause and that why 
          % we are adding it to the Tranition_States list transition_States()
            expressions(Clause_Body, % expressions()
              MethodState_Map, % methodState_Map()
              [], % delta()
              MAX_CAA_State+2, % last_Transition_State() 
              Pre_assumedState, % pre_assumedState()
              [MAX_CAA_State+2] ++ Transition_States); % transition_States()
      
      % otherwise we can pass the "clause ending pre_assumed state" in the place of 
      % Pre_assumedState (because Pre_assumedState is -1 anyways), so that it will do the 
      % changing of states for us inside the receive clause. 
      _   ->  
        Clause_Last_Transition_State = MAX_CAA_State+1,
          
        % delta_info()
        % get the delta for this clause with some addition information about the delta  
        {Delta, _, Clause_Transition_States, NoRecursion} = 
          % we pass "MAX_CAA_State+1" in the place of pre_assumedState() because "MAX_CAA_State+1"
          % is this method's clauses pre-assumed state for any receive expression inside it
            expressions(
              Clause_Body, % expressions()
              MethodState_Map, % methodState_Map()
              [], % delta()
              MAX_CAA_State+2, % last_Transition_State()
              MAX_CAA_State+1, % pre_assumedState()
              [MAX_CAA_State+2] ++ Transition_States) % transition_States()
    end,

    % when there is just recursion inside the delta.
    % "andalso NoRecursion == true" is for not to 
    % get confused with when there is no communication 
    % and because of just delta is empty.
    case Delta == [] andalso NoRecursion == false of
        % When there is transitions inside the delta
        false ->
            % add unlabelled transition to the delta
            Unlabelled_Delta = 
                addTransition(Delta, % delta()
                  Parent_Last_Transition_State, % transition_from()
                  MAX_CAA_State+1, % max_State(), we assume MAX_CAA_State+1 is the pre-assumed state
                  unlabelled), % label()
           
            % changes the last trasition state to the pre-assumed state of the clauses.
            {Changed_Delta, Changed_Transition_States, Changed_Last_Transition_State} =
                clause_changeTransitionsStates(lists:reverse(Unlabelled_Delta), % delta()
                  Pre_assumedState, % pre_assumedState()
                  MAX_CAA_State+1, % clausesPre_assumedState()()
                  MethodState_Map, % methodState_Map()
                  Clause_Transition_States, % transition_States()
                  NoRecursion), % noRecursion()

            % traverse over other clauses
            % is_Nth_clause() is true because next clause is going to be the 2nd clause 
            clause(Xs, % clauses()
              true, % many_clauses()
              true, % is_Nth_clause()
              MethodState_Map, % methodState_Map()
              lists:max(Changed_Transition_States), % lastClause_max_State()
              Changed_Last_Transition_State, % lastClause_Last_Transition_State()
              Changed_Delta, % delta()
              Parent_Last_Transition_State, % parent_Last_Transition_State()
              Pre_assumedState, % pre_assumedState()
              Changed_Transition_States, % transition_States()
              noRecursion(NoRecursion, Length_Clauses), % length_Clauses()
              MAX_CAA_State+1); % clausesPre_assumedState() 

        % when there was just a recursive call inside this clause i.e., no there no transition
         _ -> 
            % we are passing "MAX_CAA_State+1" in place of lastClause_max_State() because 
            % "MAX_CAA_State+1"  is the pre-assumed state of the clauses and there was no
            % transition.
            %  lastClause_Last_Transition_State() is -1 because there was no transition
            clause(Xs, % clauses()
              true, % many_clauses()
              true, % is_Nth_clause()
              MethodState_Map, % methodState_Map()
              MAX_CAA_State+1, % lastClause_max_State() 
              -1, % lastClause_Last_Transition_State(),
              % delta() : adding unlabelled transition to delta 
                % max_State() = Parent_Last_Transition_State-1  because we want to show an
                % unlabelled transition from parent_Last_Transition_State() to 
                % parent_Last_Transition_State() 
              addTransition(Delta, % delta()
                Parent_Last_Transition_State, % transition_from()
                Parent_Last_Transition_State-1, % max_State()
                unlabelled), % label
              Parent_Last_Transition_State, % parent_Last_Transition_State()
              Pre_assumedState, % pre_assumedState()
              Transition_States, % transition_States()
              Length_Clauses-1, % length_Clauses() : since we know there was recursion
              MAX_CAA_State+1) % clausesPre_assumedState()
end;


%%%%% end of clause %%%%%


% when we are at the Nth clause where N > 1 i.e, is_Nth_clause() == true
clause([{_,_,_,_,Clause_Body}|Xs], true, true, MethodState_Map, Max_State_lastClause, 
  Clauses_Last_Transition_State, Delta, Parent_Last_Transition_State, Pre_assumedState,
  Transition_States, Length_Clauses, ClausesPre_assumedState) ->
    % setting pre-assumed state for clauses
    case Pre_assumedState =/= -1 of
     % when there is already a pre-assumed state coming from the parent method
     true ->
        % delta_info()
        % get the delta for this clause with some addition information about the delta
        {Clause_Delta, _, Clause_Transition_States, NoRecursion} = 
        % we are passing "MAX_CAA_State+1" in place of last_Transition_State() because 
        % "MAX_CAA_State+1" is the unlabelled transition for this clause and that why 
        % we are adding it to the Tranition_States list transition_States()
            expressions(Clause_Body, % expressions()
              MethodState_Map, % methodState_Map()
              [], % delta()
              Max_State_lastClause+1, % last_Transition_State()
              Pre_assumedState, % pre_assumedState()
              [Max_State_lastClause+1|Transition_States]); % transition_States()

      % otherwise we can pass the "clause ending pre_assumed state" in the place of 
      % Pre_assumedState (because Pre_assumedState is -1 anyways), so that it will do the 
      % changing of states for us inside the receive clause.
     _   ->
        % delta_info()
        % get the delta for this clause with some addition information about the delta 
        % we are passing ClausesPre_assumedState in place of pre_assumedState() because it is 
        % pre-assumed state as Pre_assumedState (parent pre-asssumed state) is -1
        {Clause_Delta, _, Clause_Transition_States, NoRecursion} = 
            expressions(Clause_Body, % expressions()
              MethodState_Map, % methodState_Map()
              [], % delta
              Max_State_lastClause+1, % last_Transition_State()
              ClausesPre_assumedState, % pre_assumedState()
              [Max_State_lastClause+1|Transition_States]) % transition_States()
    end,

    case Clause_Delta == [] andalso NoRecursion == false of
     % When there is transitions inside the delta
     false ->
         % add unlabelled transition
        Unlabelled_Delta = 
            addTransition(Clause_Delta, % delta()
              Parent_Last_Transition_State, % transition_from()
              Max_State_lastClause,  % max_State()
              unlabelled),  % label()

        % changes the last trasition state to the pre-assumed state of the clauses.
        {Changed_Delta, Changed_Transition_States, Changed_Last_Transition_State} =
            clause_changeTransitionsStates(lists:reverse(Unlabelled_Delta), % delta()
              Pre_assumedState, % pre_assumedState()
              ClausesPre_assumedState, % clausesPre_assumedState()
              MethodState_Map, % methodState_Map()
              Clause_Transition_States, % transition_States()
              NoRecursion), % noRecursion()

        % traverse over rest of the  clauses
        clause(Xs, % clauses()
         true, % many_clauses()
         true, % is_Nth_clause()
         MethodState_Map, % methodState_Map()
         lists:max(Changed_Transition_States), % lastClause_max_State()
         Changed_Last_Transition_State, % lastClause_Last_Transition_State()
         Delta ++ Changed_Delta, % delta() : adding this clause delta to the rest.
         Parent_Last_Transition_State, % parent_Last_Transition_State()
         Pre_assumedState, % pre_assumedState()
         Changed_Transition_States, % transition_States()
         noRecursion(NoRecursion, Length_Clauses), % length_Clauses()
         ClausesPre_assumedState); % clausesPre_assumedState()

     % when there was just a recursive call inside this clause i.e., there was no transition
     _ ->  
         % we are passing Max_State_lastClause and Clauses_Last_Transition_State in place of 
         % lastClause_max_State() and lastClause_Last_Transition_State() respectively because there
         % was no transition inside this clause.
         % for delta() we are adding this clause delta (with unlabelled transition) to the rest.
        clause(Xs, % clauses()
         true, % many_clauses()
         true, % is_Nth_clause()
         MethodState_Map, % methodState_Map() 
         Max_State_lastClause, % lastClause_max_State() 
         Clauses_Last_Transition_State, % lastClause_Last_Transition_State(),
         % delta()
         Delta ++ 
          addTransition(Clause_Delta, % delta()
            Parent_Last_Transition_State, % transition_from()
            Parent_Last_Transition_State-1, % max_State()
            unlabelled), % label
         Parent_Last_Transition_State, % parent_Last_Transition_State()
         Pre_assumedState, % pre_assumedState()
         Transition_States, % transition_States()
         Length_Clauses-1, % length_Clauses() : since we know there was recursion
         ClausesPre_assumedState)  % clausesPre_assumedState()
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% -spec clause_changeTransitionsStates(delta(), pre_assumedState(), clausesPre_assumedState()(),
%         methodState_Map(), transition_States(), noRecursion()) ->
    % changes the last transition state of the clause to its pre assumed state

% The 2 clauses blow cover: when the last expression of this clause is a receive expression, then we don't have to do anything coz 
% we have already dealth with this case by now

% when there was no communication inside the delta ot method
clause_changeTransitionsStates(
 [], Parent_Pre_assumedState, _, _,
 Transition_States, _) ->
    {[],
     Transition_States,
     Parent_Pre_assumedState};


% when the last transition happened over a recevie expression
% and its last state is this clause/method parent method
% pre-assumed state.
clause_changeTransitionsStates(
 [{First_State, Label, Parent_Pre_assumedState}|Xs],
 Parent_Pre_assumedState, _, _,
 Transition_States, _) ->
    {[{First_State, Label, Parent_Pre_assumedState}|Xs],
     Transition_States,
     Parent_Pre_assumedState};

% when the last transition happened over a recevie expression
% and its last state is this clause/method pre-assumed state.
clause_changeTransitionsStates(
 [{First_State, Label, ClausesPre_assumedState}|Xs],
  _, ClausesPre_assumedState, _,
  Transition_States, _) ->
    {[{First_State, Label, ClausesPre_assumedState}|Xs],
     Transition_States,
     ClausesPre_assumedState};

% when there recursion in all the clauses
% but the recusrion call was local .
% this case cover something similar to the method  
% c1/1 in case_of.erl 
clause_changeTransitionsStates(
 [{First_State, Label, Last_State}|Xs],
  _, ClausesPre_assumedState, _,
  Transition_States, 
  false) ->
    {[{First_State, Label, Last_State}|Xs],
    Transition_States,
    ClausesPre_assumedState};

% when the transition is a send or a recursion
clause_changeTransitionsStates(
 [{First_State, Label, Last_State}|Xs],
  _, ClausesPre_assumedState, MethodState_Map,
  Transition_States, _) ->
    case 
     lists:member(Last_State, maps:values(MethodState_Map)) of
        % if the last state is not representing a recursion
        false   ->  {[{First_State, Label, ClausesPre_assumedState}|Xs], lists:delete(Last_State, Transition_States), ClausesPre_assumedState};
        _       ->  {[{First_State, Label, Last_State}|Xs], Transition_States, ClausesPre_assumedState}
end.  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%-spec expressions(expressions(),  methodState_Map(), delta(), last_Transition_State(), pre_assumedState(), transition_States()) -> delta_Info().


% when we have traversed to the end of the func() method_Form().  
expressions([], _, Delta, Last_Transition_State, _, Transition_States) ->
   {lists:reverse(Delta),
    Last_Transition_State,
    Transition_States,
    true}; % since there was no recusrion


% when we encounter a send expression
expressions([{_,_,'!',Process_ID,Data}|Xs], MethodState_Map, Delta, Last_Transition_State, Last_Pre_assumedState, Transition_States) ->
    % max transition state in the delta() so far,
    % help in "calculating" the next transition
    % state()
    Max_State = lists:max(Transition_States),
    expressions(Xs, MethodState_Map,
        % adding the send transition to the delta()
        addTransition(Delta, Last_Transition_State, Max_State, {send, Process_ID, Data}),
        Max_State+1, % the new last_Transition_State() 
        Last_Pre_assumedState,
        [Max_State+1|Transition_States] % adding the new transition state 
    );


% When there is a recursion i.e. when we 
% encounter a call expression
expressions([{call,_,{_,_,_}, _, Method_Term}|_], MethodState_Map, Delta, Last_Transition_State, _, Transition_States) -> 
    % get this call expression method starting state
    RecursionToMethod_State = maps:get(Method_Term, MethodState_Map),
    case Delta == [] of
                % when there is just recursion inside
                % this func() method_Form().
                % -1 because there is no point of 
                % last_transition_state() when a there is 
                % recursion
                true -> {[], -1, Transition_States, false}; 
                % when there was communication before
                % this recursive call
                _    -> 
                    % change the transition state of last transition(s)
                    % to RecursionToMethod_State
                    changeTransitionsStates(Delta, Last_Transition_State,
                        [],
                        RecursionToMethod_State,
                        Transition_States)
end;


expressions([{'receive', _, Body}|Xs], MethodState_Map, Delta, Last_Transition_State,  Last_Pre_assumedState, Transition_States) ->
    Max_State = lists:max(Transition_States),
    case Xs == [] andalso  Last_Pre_assumedState =/= -1 of
        % when the there is nothing after this receive block
        % and this is a nested receive block. look at example "recv6(S, Z)"
        % 2nd receive nested receive block
        true -> Pre_assumedState = Last_Pre_assumedState;
        % when this is either the very first receive block or there are 
        % other stuff after this receive block. look at example "recv(S, Z)"
        % or "recv6(S, Z)" first recive nested receive block
        _ -> Pre_assumedState = Max_State + 1   % the starting state of the next expression 
                                                % after this receive expression.
    end,
    {Recv_Delta, N_Clauses, Recv_Transition_States} = 
        receive_block(Body, Pre_assumedState, Max_State + 1, MethodState_Map, Delta, Last_Transition_State, length(Body), Transition_States),
        case N_Clauses == 0 of % when all the clauses have recursion
            true -> 
                {lists:reverse(Recv_Delta), -1, [Pre_assumedState|Recv_Transition_States], false};
            _    ->
                expressions(Xs, MethodState_Map,
                    Recv_Delta,
                    Pre_assumedState, % because Pre_assumedState is the last transition state of the receive expression
                    Last_Pre_assumedState,
                    [Pre_assumedState|Recv_Transition_States]) % adding pre-assumed state to transition_States()
end;

%-spec expressions(expressions(),  methodState_Map(), delta(), last_Transition_State(), pre_assumedState(), transition_States()) -> delta_Info().

expressions([{function, Anno, Method_Name, Arity, Clauses, Method_Term}|Xs], MethodState_Map, Delta, Last_Transition_State,  Last_Pre_assumedState, Transition_States) ->
    case
        % calling the caa/ to deal with this child method as a seprate method and 
        % get its caa_Info()
        caa({function, Anno, Method_Name, Arity, Clauses, Method_Term},
         MethodState_Map,
         Last_Transition_State,
         Last_Pre_assumedState, Transition_States) of
        % when there was no recursion inside this child method
        {{_, Func_Delta}, Func_Last_Transition_States, Func_Recv_Transition_States, true} -> 
            expressions(Xs,
             MethodState_Map, % adding this function to the map, so that global function eill have the idea of it
             lists:reverse(Func_Delta) ++ Delta , Func_Last_Transition_States,  Last_Pre_assumedState, Func_Recv_Transition_States);
        % when there is recursion
        {{_, Func_Delta}, _, Func_Recv_Transition_States, false} -> 
            {lists:reverse(Delta) ++ Func_Delta, -1, Func_Recv_Transition_States, 
             false}
    end;

% -spec clause(clauses(), many_clauses(), is_Nth_clause(),
         % methodState_Map(), lastClause_max_State(),
         % lastClause_Last_Transition_State(), caa()
         % parent_Last_Transition_State(),
         % pre_assumedState(), transition_States(),
         % length_Clauses(), clausesPre_assumedState()) -> caa_Info().

expressions([{'case', _, _, Clauses}|Xs], MethodState_Map, Delta, Last_Transition_State, Last_Pre_assumedState, Transition_States) ->
    case 
     clause(Clauses, length(Clauses) > 1, false,
      MethodState_Map, -1, -1, [], 
      Last_Transition_State, 
      Last_Pre_assumedState,
      Transition_States, length(Clauses),
      -1) of
     % when there was no recursion inside this case expression
     {{_, Case_Delta}, Case_Last_Transition_States,
      Case_Recv_Transition_States,
      true} ->
        expressions(Xs, MethodState_Map, 
         lists:reverse(Case_Delta) ++ Delta,
         Case_Last_Transition_States,  
         Last_Pre_assumedState, 
         Case_Recv_Transition_States);
        
        % when there is recursion
     {{_, Case_Delta}, _, 
      Case_Recv_Transition_States, false} -> 
        {lists:reverse(Delta) ++ Case_Delta, -1,
        Case_Recv_Transition_States,
        false}
end;      


% when none of the above
expressions([_|Xs], MethodState_Map, Delta, Last_Transition_State, Last_Pre_assumedState, Transition_States) -> 
    expressions(Xs, MethodState_Map, Delta, Last_Transition_State, Last_Pre_assumedState, Transition_States).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% -spec receive_block(receive_clauses(), pre_assumedState(), last_receive_max_state(), methodState_Map(), delta(), last_Transition_State(), n_clauses(), transition_States()) -> recv_delta_info().

receive_block([], _, _, _, Delta, _, N_Clauses, Transition_States) ->
    {Delta, N_Clauses, Transition_States};
 

receive_block([{_, _, Recv, _, Body}|Xs], Pre_assumedState, LastRecvClause_Max_State, MethodState_Map, Delta, Last_Transition_State, N_Clauses, Transition_States) ->
    % adds this receive expression clause delta form to the
    % given delta() or previous delta
    {Recv_Delta, _, Recv_Transition_States, NoRecursion} = 
        % calls expressions and pass the body of the 
        % clause.
        expressions(Body, MethodState_Map,
            addTransition(Delta, Last_Transition_State, % adds the recv transition to the delta() prior
                LastRecvClause_Max_State, {recv, Recv}),
            LastRecvClause_Max_State+1, % after the add of recv transition, its transition state is the last state now in the delta()
            Pre_assumedState,
            [LastRecvClause_Max_State+1|Transition_States]),
    % max state in the new delta
    Recv_Max_State = lists:max(Recv_Transition_States),
    % changes the last transition state of this receive clause to 
    % the pre-assumed state.
    {Pre_assumedState_Delta, Pre_assumedStateTransition_States} = 
        recv_changeTracesStates(lists:reverse(Recv_Delta), Pre_assumedState, MethodState_Map, Recv_Transition_States, NoRecursion),

    receive_block(Xs, Pre_assumedState, Recv_Max_State, % <--
        MethodState_Map,
        Pre_assumedState_Delta,
        Last_Transition_State, % because we want the "recv" transtion start state to be Last_Transition_State
        noRecursion(NoRecursion, N_Clauses), % decrease N_Clauses by 1 if there was recursion in this clause
        Pre_assumedStateTransition_States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% changes the max state of the receive clause to the pre assumed state

% -spec recv_changeTracesStates(delta(), pre_assumedState(), methodState_Map(), transition_State()) -> {delta(), transition_State()}.

recv_changeTracesStates([{First_State, Label, Last_State}|Xs], Pre_assumedState, MethodState_Map, Transition_States, NoRecursion) ->
    % no recursion == false covers the case when there was recursion but it is local.
    % e.g., in file case_of.erl method c1/1
    case lists:member(Last_State, maps:values(MethodState_Map)) orelse NoRecursion == false of
        % if the last state is not representing a recursion
        false   ->  {[{First_State, Label, Pre_assumedState}|Xs], lists:delete(Last_State, Transition_States)};
        _       ->  {[{First_State, Label, Last_State}|Xs], Transition_States}
end.      


%%%%%%%%%%%%%%%%%%%%%%%%%%

% -spec addTransition(delta(), transition_from(), max_State(), label()) -> delta().
    % addTransition/4  adds transition via given transition_from(),
    % label() and max_State() to the given delta().

addTransition(Delta, Transition_from, Max_State, Label) -> 
    [{Transition_from, Label, Max_State + 1}|Delta].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -spec changeTransitionsStates(delta(), last_Transition_State(), delta(), recursionToMethod_State(), transition_States()) -> delta_Info()
    % this changes the transtion state of the transitions in the delta() 
    % that happened before the recursion call 

changeTransitionsStates([], _, Delta, _, Transition_States) -> 
    % false and -1 because we encounter recursion
    {Delta, -1, Transition_States, false};


% transition in the delta where we have to change the
% transition state to recursion-method start state
changeTransitionsStates([{First_State, Label, Last_Transition_State}|Xs], Last_Transition_State, Delta, RecursionToMethod_State, Transition_States) ->
    changeTransitionsStates(Xs, Last_Transition_State,
        [{First_State, Label, RecursionToMethod_State}|Delta], % changing the transition state to recursionToMethod_State()
        RecursionToMethod_State, 
        lists:delete(Last_Transition_State, Transition_States));% because we don't want that transition state in our transition_States() anymore.
        
                                                                


% transitions that are no suppose to get their 
% transition state changed
changeTransitionsStates([{First_State, Label, Last_State}|Xs], Last_Transition_States, Delta, RecursionToMethod_State, Transition_States) ->
    changeTransitionsStates(Xs, Last_Transition_States, [{First_State, Label, Last_State}|Delta],
        RecursionToMethod_State, Transition_States).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -spec noRecursion(boolean(), n_clauses()) -> integer().

noRecursion(true, Length) -> Length;
noRecursion(false, Length) -> Length-1.
