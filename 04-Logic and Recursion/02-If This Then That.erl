%% # ################## #
%% # If This, Then That #
%% # ################## #

% > Adding an if construct to convert numbers into atoms
-module(drop).
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) when Distance >= 0 ->
  Gravity = case Planemo of
    earth -> 9.8;
    moon -> 1.6;
    mars -> 3.71
  end,

  Velocity = math:sqrt(2 * Gravity * Distance),
  Description = if
    Velocity == 0 -> 'stable';
    Velocity < 5 -> 'slow';
    Velocity >= 5, Velocity < 10 -> 'moving'; % The commas in the if behave like the and operator.
    Velocity >= 10 and Velocity < 20 -> 'fast';
    Velocity >= 20 -> 'speedy'
  end,

  if
    % velocity is above 40
    (Velocity > 40) -> io:format("Look out below!~n") ;
    % However, every if must find some true statement or it will report an error in those cases when nothing matches.
    true -> true
  end,

  Description.
