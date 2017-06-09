%% Every entry in an ETS tables is a tuple (or corresponding record),
%% and one piece of the tuple is designated the key.

%% ETS can hold four kinds of collections:
%% Sets ( set )
%%  Can contain only one entry with a given key. This is the default.
%% Ordered sets ( ordered_set )
%%  Same as a set, but also maintains a traversal order based on the keys. Great for
%%  anything you want to keep in alphabetic or numeric order.
%% Bags ( bag )
%%  Lets you store more than one entry with a given key. However, if you have multiple
%%  entries that have completely identical values, they get combined into a single entry.
%% Duplicate bags ( duplicate_bag )
%%  Not only lets you store more than one entry with a given key, but also lets you store
%%  multiple entries with completely identical values.

% > Setting up a simple ETS table and reporting on what’s there
-module(planemo_storage).
-export([setup/0]).
-include("records.hrl").

setup() ->
  PlanemoTable=ets:new(planemos, [named_table, {keypos, #planemo.name}]),
  ets:info(PlanemoTable).
% then:
planemo_storage:setup().
%%[{compressed,false},
%%{memory,317},
%%{owner,<0.316.0>},
%%{heir,none},
%%{name,planemos},
%%{size,0},
%%{node,nonode@nohost},
%%{named_table,true},
%%{type,set},
%%{keypos,2},
%%{protection,protected}]

% > Populating a simple ETS table and reporting on what’s there
-module(planemo_storage).
-export([setup/0]).
-include("records.hrl").

setup() ->
  PlanemoTable=ets:new(planemos, [named_table, {keypos, #planemo.name}]),
  ets:insert(planemos,
    #planemo{ name=mercury, gravity=3.7, diameter=4878, distance_from_sun=57.9 }),
  ets:insert(PlanemoTable,
    #planemo{ name=venus, gravity=8.9, diameter=12104, distance_from_sun=108.2 }),
  ets:insert(PlanemoTable,
    #planemo{ name=earth, gravity=9.8, diameter=12756, distance_from_sun=149.6 }),
  ets:insert(PlanemoTable,
    #planemo{ name=moon, gravity=1.6, diameter=3475, distance_from_sun=149.6 }),
  ets:insert(PlanemoTable,
    #planemo{ name=mars, gravity=3.7, diameter=6787, distance_from_sun=227.9 }),
  ets:insert(PlanemoTable,
    #planemo{ name=ceres, gravity=0.27, diameter=950, distance_from_sun=413.7 }),
  ets:insert(PlanemoTable,
    #planemo{ name=jupiter, gravity=23.1, diameter=142796, distance_from_sun=778.3 }),
  ets:info(PlanemoTable).

