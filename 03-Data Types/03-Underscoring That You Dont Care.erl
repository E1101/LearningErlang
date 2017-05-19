%% You could create variables for arguments and then never use them, but you’ll get warn‐
%% ings from the compiler.

-module(drop).
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) -> math:sqrt(2 * 9.8 * Distance).
% Warning: variable 'Planemo' is unused

% fix with:
fall_velocity(_, Distance) -> math:sqrt(2 * 9.8 * Distance).


%% !! You can also start variables with underscores—like _Planemo —and the
%%    compiler won’t warn if you never use those variables.

