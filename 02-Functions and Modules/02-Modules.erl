%% Modules are a more formal place to put programs, and they give
%% you the ability to store, encapsulate, share, and manage
%% your code more effectively.


%% # ############## #
%% # Define Modules #
%% # ############## #

% > Module for calculating and converting fall velocities [drop.el]
% ! module with name "drop" should declared in file names "drop.el"
-module(drop).
% which functions it should make visible to other code that uses this module.
% arity /1 mean fall_velocity with one(1) argument.
% Erlang considers functions with the same name but different arity to be different functions.
-export([fall_velocity/1, mps_to_mph/1, mps_to_kph/1]).

% ! function names start with lowercase, not uppercase.
% ! fun and end don’t appear.
% FallVelocity = fun(Distance) -> math:sqrt(2 * 9.8 * Distance) end.
fall_velocity(Distance) -> math:sqrt(2 * 9.8 * Distance).

mps_to_mph(Mps) -> 2.23693629 * Mps.

mps_to_kph(Mps) -> 3.6 * Mps.


% > The shell will let you compile modules and then use them immediately.
% ! You need to be in the same directory as the file
ls().    % drop.erl
c(drop). % dont need to give .erl extension
ls().    % drop.erl drop.beam


% > Call modules functions
drop:fall_velocity(20).
drop:mps_to_mph(drop:fall_velocity(20)).


%% # ######################################### #
%% # Modules that call code from other modules #
%% # ######################################### #

-module(combined).
-export([height_to_mph/1]).

height_to_mph(Meters) -> convert:mps_to_mph(drop:fall_velocity(Meters)).


% > Using -import directive to reduce complexity
% ! not to use it unless you just can’t resist. It can make it harder to figure
% !-where bugs are coming from, which may cost you more time than the extra typing.
-module(combined).
-export([height_to_mph/1]).
-import(drop, [fall_velocity/1]).
-import(convert, [mps_to_mph/1]).

height_to_mph(Meters) -> mps_to_mph(fall_velocity(Meters)).

