%% # ########################################### #
%% # Functions and Pattern Matching over Records #
%% # ########################################### #

% > Suppose you want to define the birthday function, which increases the age of the person by one.
birthday(P) ->
  P#person{age = P#person.age + 1}.

% ! But it is clearer to use pattern matching:
birthday(#person{age=Age} = P) ->
  P#person{age=Age+1}.

% > It is also possible to match against field values so that you increase only Joe’s age,
% - keeping everyone else the same age:
joesBirthday(#person{age=Age,name="Joe"} = P) ->
  P#person{age=Age+1};
joesBirthday(P) -> P.


% > Nested Records

-record(name, {first, surname}).

P = #person{name = #name{first = "Robert", surname = "Virding"}}
First = (P#person.name)#name.first.


% > A method that pattern matches a complete record
-module(record_drop).
-export([fall_velocity/1]).
-include("records.hrl").

fall_velocity(#tower{} = T) -> fall_velocity(T#tower.planemo, T#tower.height).
fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2 * 3.71 * Distance).
% then:
record_drop:fall_velocity(Tower5).


% > A method that pattern matches components of a record
-module(record_drop).
-export([fall_velocity/1]).
-include("records.hrl").

fall_velocity(#tower{planemo=Planemo, height=Distance}) -> fall_velocity(Planemo, Distance).
fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2 * 3.71 * Distance).


% > A method that pattern matches the whole record as well as components of a record
-module(record_drop).
-export([fall_velocity/1]).
-include("records.hrl").

fall_velocity(#tower{planemo=Planemo, height=Distance} = T) ->
  io:format("From ~s's elevation of ~p meters on ~p, the object will reach ~p m/s before crashing in ~s.~n",
    [T#tower.name, Distance, Planemo, fall_velocity(Planemo, Distance), T#tower.location ]).

fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2 * 3.71 * Distance).

