% ! Maps were made available from version R17 of Erlang.

F1 = #{ a => 1, b => 2 }.

Facts = #{ {wife,fred} => "Sue", {age, fred} => 45,
  {daughter,fred} => "Mary",
  {likes, jim} => [...]
}.


% Internally the map is stored as an ordered collection.
F2 = #{ b => 2, a => 1 }.
% #{a => 1, b => 2 }.
F1 = F2.
% #{a => 1, b => 2 }.

F3 = F1#{ c => xx }.
F4 = F1#{ c := 3}    % is used to update the value of an existing key
%% ** exception error: bad argument
%% key c does not exist in old map


% Pattern Matching >

Henry8 = #{ class => king, born => 1491, died => 1547 }.

#{ born => B } = Henry8.
B. % 1491


% > function count_characters(Str) that returns a map of the number of times a particular
%   character occurs in a string.

count_characters(Str) ->
  count_characters(Str, #{}).

count_characters([H|T], #{ H => N }=X) ->
  count_characters(T, X#{ H := N+1 });
count_characters([H|T], X) ->
  count_characters(T, X#{ H => 1 });
count_characters([], X) ->
  X.

%then:

count_characters("hello").
% #{101=>1,104=>1,108=>2,111=>1}
%% h (ASCII, 101)


%% # ######################### #
%% # BIFs That Operate on Maps #
%% # ######################### #

erlang:is_map(M) -> bool()

maps:to_list(M) -> [{K1,V1},..., {Kn,Vn}]

maps:from_list([{K1,V1},...,{Kn,Vn}]) -> M

maps:map_size(Map) -> NumberOfEntries

maps:is_key(Key, Map) -> bool()

maps:get(Key, Map) -> Val

maps:find(Key, Map) -> {ok, Value} | error

maps:keys(Map) -> [Key1,..KeyN]

maps:remove(Key, M) -> M1

maps:without([Key1, ..., KeyN], M) -> M1

maps:difference(M1, M2) -> M3
% M3 is equivalent to M1 with any elements having the same keys as the
% elements in M2 removed.

