%% # ############################# #
%% # Creating the Initial Database #
%% # ############################# #

%%  Before we can do anything, we have to create an Mnesia database. You need
%%  to do this only once.

net_adm:ping(switch@Vaio). % pong
nodes(). [switch@Vaio]
% has to be executed on one of the connected nodes only
% will propagate to the other nodes automatically.

mnesia:create_schema([node()|nodes()]).
ls().
% Mnesia.om@Vaio Mnesia.switch@Vaio include
% lib
% ok

% If you do not plan to run Mnesia in a distributed environment, the schema directory name will be
% Mnesia.nonode@nohost . Just pass [node()] as an argument to the create_schema/1 call.
mnesia:create_schema([node()]).

% > override the location of the root directory
%   Or we can point to a specific database when we start Erlang.
$ erl -mnesia dir Dir

$ erl -mnesia dir '"/home/joe/some/path/to/Mnesia.company"'
mnesia:create_schema([node()]).

% ! If you start Mnesia without a schema, a memory-only database will be created.


%% # ############### #
%% # Starting Mnesia #
%% # ############### #

application:start(mnesia).

% where you are not using OTP behaviors
mnesia:start() .

% > in case we using boot scripts, include the Mnesia application in your release file.


application:stop(mnesia).
mnesia:stop() .


%% # ############# #
%% # Mnesia Tables #
%% # ############# #

% ! Note: The primary key of the shop table is the first column in the table.

mnesia:create_table(Name, Options)

%% - Options
%%   {disc_copies, Nodelist}
%%     Provides the list of nodes where you want disc and RAM replicas of the table.
%%   {disc_only_copies, Nodelist}
%%     Nodelist contains the nodes where you want disc-only copies of this particular
%%     table. This is usually a backup node, as local reads will be slow on these nodes.
%%   {ram_copies, Nodelist}
%%     Specifies which nodes you want to have duplicate RAM copies of this particular
%%     table. The default value of this attribute is [node()] , so omitting it will create a local
%%     Mnesia RAM copy.
%%   {type, Type}
%%     States whether the table is a set , ordered_set , or bag . The default value is set .
%%   {attributes, AtomList}
%%     list of atoms denoting the record field names.
%%     They are mainly used when indexing or using query list comprehensions.
%%     do not hardcode them; generate them using the function call record_info(fields, RecordName) .
%%   {index, List}
%%     list of attributes (record field names) which can be used as secondary keys
%%     when accessing elements in the table.


rr(usr).
Fields = record_info(fields, usr).
% [msisdn,id,status,plan,services]

application:start(mnesia).
mnesia:create_table(usr, [{disc_copies, [node()]},
  {ram_copies, nodes()}, {type, set}, {attributes, Fields}, {index, [id]}]).

% > For large persistent tables, or tables that were incorrectly closed and whose backup files need repair
mnesia:wait_for_tables(TableList, TimeOut)
% {timeout, TableList} ignore this and continue assuming table loaded.


% --- test_mnesia.erl -------------------------------
-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).


%% we need to create a database schema, start the database,
%% add some table definitions and stop the database, and
%% restart it. We need to do this only once.

do_this_once() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(shop,
    [{attributes, record_info(fields, shop)}]),
  mnesia:create_table(cost,
    [{attributes, record_info(fields, cost)}]),
  mnesia:create_table(design, [{attributes, record_info(fields, design)}]),
  mnesia:stop().

