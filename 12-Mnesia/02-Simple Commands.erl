example_tables() ->
  [ %% The shop table
    {shop, apple, 20, 2.3}
    ,{shop, orange, 100, 3.8}
    ,{shop, pear, 200, 3.6}
    ,{shop, banana, 420, 4.5}
    ,{shop, potato, 2456, 1.2}
    %% The cost table
    ,{cost, apple, 1.5}
    ,{cost, orange, 2.4}
    ,{cost, pear, 2.2}
    ,{cost, banana, 1.5}
    ,{cost, potato, 0.6}
  ].

reset_tables() ->
  mnesia:clear_table(shop),
  mnesia:clear_table(cost),
  F = fun() ->
    foreach(fun mnesia:write/1, example_tables())
  end,

  mnesia:transaction(F).

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.



%% # ############################# #
%% # Selecting All Data in a Table #
%% # ############################# #

%% SQL equivalent
%% SELECT * FROM shop;
demo(select_shop) ->
  %% qlc:q compiles the query (its parameter) into an internal form that is used to query the database.
  %% the list of X such that X is taken from the shop Mnesia table.
  do(qlc:q([X || X <- mnesia:table(shop)]));

% !! the following code is not equivalent to the code in the example:
     Var = [X || X <- mnesia:table(shop)],
     qlc:q(Var). % must be a list comprehension literal


%% # ########################## #
%% # Choosing Data from a Table #
%% # ########################## #

%% SQL equivalent
%% SELECT item, quantity FROM shop;
demo(select_some) ->
  do(qlc:q([{X#shop.item, X#shop.quantity} || X <- mnesia:table(shop)]));


%% # ######################################### #
%% # Conditionally Selecting Data from a Table #
%% # ######################################### #

%% SQL equivalent
%% SELECT shop.item FROM shop
%% WHERE shop.quantity < 250;
demo(reorder) ->
  do(qlc:q([X#shop.item || X <- mnesia:table(shop),
                                X#shop.quantity < 250
  ]));


%% # ###################################### #
%% # Selecting Data from Two Tables (Joins) #
%% # ###################################### #

%% SQL equivalent
%% SELECT shop.item
%% FROM shop, cost
%% WHERE shop.item = cost.name
%%   AND cost.price < 2
%%   AND shop.quantity < 250

demo(join) ->
  do(qlc:q([X#shop.item || X <- mnesia:table(shop),
    X#shop.quantity < 250,
    Y <- mnesia:table(cost),
    X#shop.item =:= Y#cost.name,
    Y#cost.price < 2
  ])).


%% # ######################################## #
%% # Adding and Removing Data in the Database #
%% # ######################################## #

add_shop_item(Name, Quantity, Cost) ->
  Row = #shop{item=Name, quantity=Quantity, cost=Cost},
  F = fun() ->
    mnesia:write(Row)
  end,
  mnesia:transaction(F).

%% ! If the newly created record has the same primary key as an existing row in the
%%   database table, it will overwrite that row; otherwise, a new row will be created.


%% # ############## #
%% # Removing a Row #
%% # ############## #

%% To remove a row, we need to know the object ID (OID) of the row. This is
%% formed from the table name and the value of the primary key.

remove_shop_item(Item) ->
  Oid = {shop, Item},
  F = fun() ->
    mnesia:delete(Oid)
  end,
  mnesia:transaction(F).

