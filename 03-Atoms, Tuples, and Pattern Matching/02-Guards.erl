
% > keep negative numbers away from the square root function
-module(drop).
-export([fall_velocity/2]).

fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2 * 3.71 * Distance).

% then
drop:fall_velocity(earth,-20). % exception error: no function clause matching


% > Calculating absolute value with guards
-module(mathdemo).
-export([absolute_value/1]).

absolute_value(Number) when Number < 0 -> -Number;
absolute_value(Number) when Number == 0 -> 0;
absolute_value(Number) when Number > 0 -> Number.

% even better

absolute_value(Number) when Number < 0 -> -Number;
absolute_value(0) -> 0;
absolute_value(Number) -> Number.
