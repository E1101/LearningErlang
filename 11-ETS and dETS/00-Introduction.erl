%% ETS is short for Erlang term storage, and
%% DETS is short for disk ETS.

%% ETS and DETS tables store tuples. One of the elements in the tuple (by default,
%% the first) is called the key of the table. We insert tuples into the table and
%% extract tuples from the table based on the key.

%% An ETS table is said to be owned by the process
%% that created it—when that process dies

%% In a set, all the keys in the different tuples in the table must be unique.
%% In an ordered set, the tuples are sorted. In a bag there can be more than one tuple with the
%% same key, but no two tuples in the bag can be identical. In a duplicate bag
%% several tuples can have the same key, and the same tuple can occur many
%% times in the same table.


% > illustrate how these work with the following little test program:

-module(ets_test).
-export([start/0]).

start() ->
  lists:foreach(fun test_ets/1,
    [set, ordered_set, bag, duplicate_bag]).

test_ets(Mode) ->
  TableId = ets:new(test, [Mode]),
  ets:insert(TableId, {a,1}),
  ets:insert(TableId, {b,2}),
  ets:insert(TableId, {a,1}),
  ets:insert(TableId, {a,3}),
  List = ets:tab2list(TableId),
  io:format("~-13w => ~p~n", [Mode, List]),
  ets:delete(TableId).

% then:
ets_test:start().
%% set           => [{b,2},{a,3}]
%% ordered_set   => [{a,3},{b,2}]
%% bag           => [{b,2},{a,1},{a,3}]
%% duplicate_bag => [{b,2},{a,1},{a,1},{a,3}]

