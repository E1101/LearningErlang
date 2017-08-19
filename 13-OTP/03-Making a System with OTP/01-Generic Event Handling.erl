-module(event_handler).
-export([make/1, add_handler/2, event/2]).

%% make a new event handler called Name
%% the handler function is no_op -- so we do nothing with the event
make(Name) ->
  %% Make a “do nothing” event handler called Name (an atom).
  %% This provides a place to send events to.
  register(Name, spawn(fun() -> my_handler(fun no_op/1) end)).

%% Add a handler Fun to the event handler called Name.
%% Now when an event X occurs, the event handler will evaluate Fun(X) .
add_handler(Name, Fun) -> Name ! {add, Fun}.

%% generate an event
%% Send the event X to the event handler called Name .
event(Name, X) -> Name ! {event, X}.

my_handler(Fun) ->
  receive
    {add, Fun1} ->
      my_handler(Fun1);
    {event, Any} ->
      (catch Fun(Any)),
      my_handler(Fun)
  end.

no_op(_) -> void.


% Here’s the code for an event handler callback module:

-module(motor_controller).
-export([add_event_handler/0]).

add_event_handler() ->
  event_handler:add_handler(errors, fun controller/1).

controller(too_hot) ->
  io:format("Turn off the motor~n");

controller(X) ->
  io:format("~w ignored event: ~p~n",[?MODULE, X]).


% Then:

motor_controller:add_event_handler().

event_handler:event(errors, cool).
% motor_controller ignored event: cool

event_handler:event(errors, too_hot).
% Turn off the motor

