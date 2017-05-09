%% # ################ #
%% # Evaluating Cases #
%% # ################ #

% > Moving pattern matching inside the function
-module('01-Evaluating Cases').
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) when Distance >= 0 ->
  case Planemo of
    earth -> math:sqrt(2 * 9.8 * Distance);
    moon -> math:sqrt(2 * 1.6 * Distance);
    mars -> math:sqrt(2 * 3.71 * Distance) % no closing period!
end.


% > Using the return value of the case construct to clean up the function
-module('01-Evaluating Cases').
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) when Distance >= 0 ->
  Gravity = case Planemo of
    earth -> 9.8;
    moon -> 1.6;
    mars -> 3.71
  end, % note comma - function isn't done yet
  math:sqrt(2 * Gravity * Distance).


% > Moving guards into the case statement
-module('01-Evaluating Cases').
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) ->
  Gravity = case Planemo of
      earth when Distance >= 0 -> 9.8;
      moon when Distance >= 0 -> 1.6;
      mars when Distance >= 0 -> 3.71
  end, % note comma - function isn't done yet
  math:sqrt(2 * Gravity * Distance).
