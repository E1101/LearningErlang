%% atoms are bits of text that start with a lowercase letter, like ok or earth .
this_is_a_short_sentence.
% or
me@home.

'Today is a goodday'.

% Atoms have a value—it’s the same as their text.
hello %  hello


%% # ########################### #
%% # Pattern Matching with Atoms #
%% # ########################### #

% > Pattern matching on atoms as well as function names
-module(drop).
-export([fall_velocity/2]).

fall_velocity(earth, Distance) -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) -> math:sqrt(2 * 3.71 * Distance).

% then
drop:fall_velocity(earth,20).


%% # ############### #
%% # Atomic Booleans #
%% # ############### #

% atoms have special properties: true and false

3<2. % false
3>2. % true

true or false.
true and true.
true xor true. % false
not true. % false

