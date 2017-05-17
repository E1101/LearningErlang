%
% > An overly simple process definition
-module(bounce).
-export([report/0]).

report() ->
  receive
    X -> io:format("Received ~p~n",[X])
  end.

%% then:
Pid=spawn(bounce,report,[]). % turns the function into a free-standing process.
% <0.38.0>
Pid ! 23. % Received 23 | but report() exit after this and no longer listening


% > A function that creates a stable process
-module(bounce).
-export([report/0]).

report() ->
  receive
    X -> io:format("Received ~p~n",[X]),
      report() % after the function shows the message that arrived, it will run again.
  end.


% > A function that adds a counter to its message reporting
-module(bounce).
-export([report/1]).

report(Count) ->
  receive
    X -> io:format("Received #~p: ~p~n",[Count,X]),
      report(Count+1)
  end.


% > Using the return value of the receive clause as state for the next iteration
-module(bounce).
-export([report/1]).

report(Count) ->
  NewCount = receive
     X -> io:format("Received #~p: ~p~n",[Count,X]),
           Count + 1
  end,
  report (NewCount).


% > Registering a Process
Pid1=spawn(bounce,report,[1]).
register(bounce,Pid1). % true
bounce ! hello.

GetBounce = whereis(bounce). % <0.33.0>
unregister(bounce). % true
TestBounce = whereis(bounce). % undefined
