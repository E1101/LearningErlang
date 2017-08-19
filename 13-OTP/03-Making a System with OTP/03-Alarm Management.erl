% The alarm handler is a callback module for the OTP gen_event behavior.

-module(my_alarm_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, code_change/3, handle_event/2, handle_call/2,
  handle_info/2, terminate/2]).

%% init(Args) must return {ok, State}
init(Args) ->
  io:format("*** my_alarm_handler init:~p~n",[Args]),
  {ok, 0}.

% This should return {ok, NewState} .
handle_event({set_alarm, tooHot}, N) ->
  error_logger:error_msg("*** Tell the Engineer to turn on the fan~n"),
  {ok, N+1};

handle_event({clear_alarm, tooHot}, N) ->
  error_logger:error_msg("*** Danger over. Turn off the fan~n"),
  {ok, N};

handle_event(Event, N) ->
  io:format("*** unmatched event:~p~n",[Event]),
  {ok, N}.

handle_call(_Request, N) -> Reply = N, {ok, Reply, N}.

handle_info(_Info, N) -> {ok, N}.

terminate(_Reason, _N) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


% then:

$ erl -boot start_sasl -config elog3

alarm_handler:set_alarm(tooHot).
% ok

gen_event:swap_handler(alarm_handler,
  {alarm_handler, swap},
  {my_alarm_handler, xyz}).

alarm_handler:set_alarm(tooHot).
%% *** Tell the Engineer to turn on the fan

alarm_handler:clear_alarm(tooHot).
%% *** Danger over. Turn off the fan

