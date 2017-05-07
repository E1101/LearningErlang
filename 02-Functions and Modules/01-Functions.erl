%% # ################# #
%% # Create a function #
%% # ################# #

% > binding the function to the variable
FallVelocity = fun(Distance) -> math:sqrt(2 * 9.8 * Distance) end.

FallVelocity(20). % 19.79898987322333


%% # ################## #
%% # From Module to Fun #
%% # ################## #

F_v = fun drop:fall_velocity/1.
F_v(20).

