
% > Create Table

ets:new(myTable, Opts) % returns the table identifier used to reference the table.

%% when no options is passed, create a set, with the key in position 1,
%% and providing protected access to the values of the table.

%% ! Protected access allows all processes to read the table,
%%   but only the owner to write to it.

%% Other options include:
%% - set , ordered_set , bag , duplicate_bag
%% - {keypos, Pos}
%% - public , protected , private
%% - named_table
%% !  If the table is created with the named_table option set, you can access it using either the
%% -  name or the table identifier.


TabId = ets:new(myTable, []).
ets:info(TabId).
%% [{memory,301},
%% {owner,<0.31.0>},
%% {name,myTable},
%% {size,0},
%% {node,nonode@nohost},
%% {named_table,false},
%% {type,set},
%% {keypos,1},
%% {protection,protected}]


% > Delete Table

ets:delete(TabId) .
%% a table is linked to the process that created it,
%% and if the process terminates, the table is deleted automatically.


%% # ####################### #
%% # Handling Table Elements #
%% # ####################### #

ets:insert(TabId,{alison,sweden}). % true
ets:lookup(TabId,alison). % [{alison,sweden}]

ets:insert(TabId,{alison,italy}).
ets:lookup(TabId,alison). % [{alison,italy}] overwrite in sets

ets:delete(TabId).

TabId2 = ets:new(myTable,[bag]).
ets:insert(TabId2,{alison,sweden}).
ets:insert(TabId2,{alison,italy}).
ets:lookup(TabId2,alison). % [{alison,sweden},{alison,italy}]


%% # ################################### #
%% # Extracting Table Information: match #
%% # ################################### #

ets:new(countries, [bag,named_table]).
ets:insert(countries,{yves,france,cook}).
ets:insert(countries,{sean,ireland,bartender}).
ets:insert(countries,{marco,italy,cook}).
ets:insert(countries,{chris,ireland,tester}).

ets:match(countries,{'$1',ireland,'_'}).
%% [[sean],[chris]]
ets:match(countries,{'$1','$0',cook}).
%% [[france,yves],[italy,marco]]

%% • '_' , which is a wildcard that will match any value in this position
%% • '$0' and '$1' , which are variables that will match any value in this position
%% • A value, in this case something such as ireland or cook

% > return the entire tuple matching a pattern

ets:match_object(countries,{'_',ireland,'_'}).
% [{sean,ireland,bartender},{chris,ireland,tester}]

NewTab = ets:match_delete(countries,{'_',ireland,'_'}).
ets:match_object(countries,{'_',ireland,'_'}).
% []

%% Note:
%% a match operation on a large table can therefore stop other processes from
%% executing until the operation has traversed the whole table.
%% To avoid this problem, it is best to work by table traversal using first
%% and next , as shown earlier. It might take more time, but it will not dis-
%% rupt the real-time properties of your system.

