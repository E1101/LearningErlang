do_something(...) ->
  F = fun() ->
    % ...
    mnesia:write(Row)
    % ... or ...
    mnesia:delete(Oid)
    % ... or ...
    qlc:e(Q)
  end,

  mnesia:transaction(F)

%% F is a fun with zero arguments. Inside F we called some combination of mne-
%% sia:write/1 , mnesia:delete/1 , or qlc:e(Q) (where Q is a query compiled with qlc:q/1 ).
%% Having built the fun, we call mnesia:transaction(F) , which evaluates the expression
%% sequence in the fun.

%% Transactions guard against faulty program code but more importantly against
%% concurrent access of the database. Suppose we have two processes that try
%% to simultaneously access the same data. For example, suppose I have $10 in
%% my bank account. Now suppose two people try to simultaneously withdraw
%% $8 from that account. I would like one of these transactions to succeed and
%% the other to fail.


%% # ###################### #
%% # Aborting a Transaction #
%% # ###################### #

farmer(Nwant) ->
  %% Nwant = Number of oranges the farmer wants to buy
  F = fun() ->
    %% find the number of apples
    [Apple] = mnesia:read({shop,apple}),
    Napples = Apple#shop.quantity,
    Apple1 = Apple#shop{quantity = Napples + 2*Nwant},
    %% update the database
    mnesia:write(Apple1),
    %% find the number of oranges
    [Orange] = mnesia:read({shop,orange}),
    NOranges = Orange#shop.quantity,

    if
      NOranges >= Nwant ->
        N1 = NOranges - Nwant,
        Orange1 = Orange#shop{quantity=N1},
        %% update the database
        mnesia:write(Orange1);
      true ->
        %% Oops -- not enough oranges
        mnesia:abort(oranges)
    end
  end,

  mnesia:transaction(F).

