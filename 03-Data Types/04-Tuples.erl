%% # ######################## #
%% # Adding Structure: Tuples #
%% # ######################## #

%% it’s common to use an atom as the
%% first element of the tuple, which describes what the tuple represents. So, we’d
%% write {point, 10, 45} instead of {10, 45} , which makes the program a lot more
%% understandable.

Person = {person, {name, joe}, {height, 1.82},
   {footsize, 42}, {eyecolour, brown}}.


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

Point = {point, 10, 45}.
{point, X, Y} = Point.
X. % 10
Y. % 45

Person={person,{name,joe,armstrong},{footsize,42}}.
{_,{_,Who,_},_} = Person.
Who. % joe


% > Example:

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


% > tag convention used to represent different types of data
% - have a meaning in the program that uses it.

{person, 'Joe', 'Armstrong'}
%% the atom person is the tag and might denote that
%% the second field in the tuple is always the first
%% name of the person, while the third is the surname.