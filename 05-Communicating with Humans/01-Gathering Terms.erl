%% # ############### #
%% # Gathering Terms #
%% # ############### #

% > Asking the user for an Erlang term
-module('01-Gathering Terms').
-export([term/0]).

term() ->
  Input = io:read("What {planemo, distance} ? >>"),
  Term = element(2,Input), % it will contain a tuple like {ok,{mars,20}}
  drop:fall_velocity(Term).

%% ask:term().
% What {planemo, distance} ? >>{mars,20}.
% 12.181953866272849


% > Asking the user for an Erlang term and handling bad results
-module('01-Gathering Terms').
-export([term/0]).

term() ->
  Input = io:read("What {planemo, distance} ? >>"),
  process_term(Input).

process_term({ok, Term}) when is_tuple(Term) -> drop:fall_velocity(Term);
process_term({ok, _}) -> io:format("You must enter a tuple.~n");
process_term({error, _}) -> io:format("You must enter a tuple with correct syntax.~n").


% > Asking the user for an Erlang term and handling bad results
-module('01-Gathering Terms').
-export([term/0]).

term() ->
  Input = io:read("What {planemo, distance} ? >>"),
  process_term(Input).

process_term({ok, Term}) when is_tuple(Term) ->
  Velocity = drop:fall_velocity(Term),
  io:format("Yields ~w. ~n",[Velocity]),
  term();

process_term({ok, quit}) ->
  io:format("Goodbye.~n");
  % does not call term() again

process_term({ok, _}) ->
  io:format("You must enter a tuple.~n"),
  term();

process_term({error, _}) ->
  io:format("You must enter a tuple with correct syntax.~n"),
  term().
