
dets:open_file(TableName , Options)

%% Options argument:
%% - {auto_save, Interval}
%% - {file, FileName}
%%   to override the default name of the table as a filename and provide a location
%%   in which to save.
%% - {repair, Bool}
%%   whether the table should be repaired if it was not properly closed. If repair
%%   is needed, setting Bool to true will trigger the repair automatically, whereas
%%   false will return the tuple {error, need_repair}.
%% - {type, TableType}
%%   set , bag , or duplicate_bag . Ordered sets are currently not supported!
%% - {ram_file, Bool}
%%   enhance performance if you need to populate the table with lots of elements.
%%   It stores the elements in RAM and spools them to file either when you call
%%   dets:sync(Name) or when you close the table. The flag is set to false by default.


dets:open_file(food, [{type, bag}, {file, "/Users/Francesco/food"}]).

dets:insert(food, {italy, spaghetti}).
dets:insert(food, {sweden, meatballs}).

dets:lookup(food, china). % []

dets:insert(food, {italy, pizza}).

NotItalian = ets:fun2ms(fun({Loc, Food}) when Loc /= italy -> Food end).
% [{{'$1','$2'},[{'/=','$1',italy}],['$2']}]
dets:select(food, NotItalian). % [meatballs]

dets:close(food).


{ok, Ref} = dets:open_file("/Users/Francesco/food").

dets:lookup(Ref, italy).
% [{italy,spaghetti},{italy,pizza}]

dets:info(Ref).
%% [{type,bag},
%% {keypos,1},
%% {size,3},
%% {file_size,5920},
%% {filename,"/Users/Francesco/food"}]
