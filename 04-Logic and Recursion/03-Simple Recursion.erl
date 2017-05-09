%% # ################ #
%% # Simple Recursion #
%% # ################ #

% Because variables can’t change values,
% the main tool you’ll use to repeat actions is recursion.

% > Counting down
-module(count).
-export([countdown/1]).

countdown(From) when From > 0 ->
  io:format("~w!~n", [From]),
  countdown(From-1);

countdown(From) ->
  io:format("blastoff!~n").


% > Counting Up
-module(count).
-export([countup/1]).

countup(Limit) ->
  countup(1, Limit).

countup(Count, Limit) when Count =< Limit ->
  io:format("~w!~n", [Count]),
  countup(Count+1, Limit);

countup(Count, Limit) ->
  io:format("Finished.~n").


% > A factorial written with the counting down approach
-module(fact).
-export([factorial/1]).

factorial(N) when N > 1->
  N * factorial(N-1);

factorial(N) when N =< 1 ->
  1.

% > A factorial written with the counting up approach
-module(fact).
-export([factorial/1]).

factorial(N) ->
  factorial(1, N, 1).

factorial(Current, N, Result) when Current =< N ->
  NewResult = Result*Current,
  io:format("~w yields ~w!~n", [Current, NewResult]),
  factorial(Current+1, N, NewResult);

factorial(Current, N, Result) ->
  io:format("Finished.~n"),
  Result.

