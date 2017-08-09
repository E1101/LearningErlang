%% One of the disadvantages of using a conventional DBMS is that there are a
%% limited number of data types you can store in a table column. You can store
%% an integer, a string, a float, and so on. But if you want to store a complex
%% object, then you’re in trouble. So, for example, if you’re a Java programmer,
%% storing a Java object in a SQL database is pretty messy.

%% Mnesia is designed to store Erlang data structures. In fact, you can store any
%% Erlang data structure you want in an Mnesia table.

%% there is no impedance mismatch between the data
%% structures in the database and the data structures in our programming lan-
%% guage. This means that inserting and deleting complex data structures into
%% the database is very fast.


-record(design, {id, plan}).

D1 = #design{
  id   = {joe,1},
  plan = {circle,10}
},
D2 = #design{
  id   = fred,
  plan = {rectangle,10,5}
},
D3 = #design{
  id   = {jane,{house,23}},
  plan = {house,
            [{floor,1,
              [{doors,3},
               {windows,12},
               {rooms,5}]},
             {floor,2,
              [{doors,2},
               {rooms,4},
               {windows,15}]}
            ]}
},
F = fun() ->
  mnesia:write(D1),
  mnesia:write(D2),
  mnesia:write(D3)
end,

mnesia:transaction(F).

get_plan(PlanId) ->
  F = fun() -> mnesia:read({design, PlanId}) end,
  mnesia:transaction(F).

% then

test_mnesia:get_plan(fred).
% {atomic,[{design,fred,{rectangle,10,5}}]}

test_mnesia:get_plan({jane, {house,23}}).
%% {atomic,[{design,{jane,{house,23}},
%% {house,[{floor,1,[{doors,3},
%% {windows,12},
%% {rooms,5}]},
%% {floor,2,[{doors,2},
%% {rooms,4},
%% {windows,15}]}]}}]}
