%% # ######################## #
%% # Adding Structure: Tuples #
%% # ######################## #

% Tuples can contain any kind of Erlang data, including
% numbers, atoms, other tuples, and the lists and strings
{earth, 20}.

Tuple = {earth, 20}.
element(2, Tuple). % 20

NewTuple = setelement(2, Tuple, 40). % {earth,40}
tuple_size(NewTuple). % 2


%% # ############################ #
%% # Pattern Matching with Tuples #
%% # ############################ #

-module(drop).
-export([fall_velocity/1]).

fall_velocity({earth, Distance}) -> math:sqrt(2 * 9.8 * Distance);
fall_velocity({moon, Distance}) -> math:sqrt(2 * 1.6 * Distance);
fall_velocity({mars, Distance}) -> math:sqrt(2 * 3.71 * Distance).

% then:
drop:fall_velocity({earth,20}).


% > Encapsulating arguments in a tuple and passing them to a private function
-module(drop).
-export([fall_velocity/1]).

fall_velocity({Planemo, Distance}) -> fall_velocity(Planemo, Distance).

fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2 * 3.71 * Distance).

