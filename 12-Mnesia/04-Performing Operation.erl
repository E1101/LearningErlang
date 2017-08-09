%% # ############ #
%% # Transactions #
%% # ############ #

%% transaction guarantees that the database will be taken from one consistent state
%% to another, that changes are persistent and atomic across all nodes, and that transactions
%% running in parallel will not interfere with each other.
mnesia:transaction(Fun) % fun contains operations read , write , and delete.
% {atomic, Result} | {aborted, Reason}


%% # ####### #
%% # Writing #
%% ######### #

Rec = #usr{msisdn=700000003, id=3, status=enabled,
  plan=prepay, services=[data,sms,lbs]}.


mnesia:transaction(fun() -> mnesia:write(Rec) end).
% {atomic, OK}


%% # #################### #
%% # Reading and Deleting #
%% ###################### #

mnesia:transaction(fun() -> mnesia:read({usr, 700000003}) end). % {TableName, Key}
% {atomic,[#usr{msisdn = 700000003,id = 3,status = enabled, plan = prepay,
%    services = [data,sms,lbs]}]}

mnesia:read({usr, 700000003}).
% ** exception exit: {aborted,no_transaction}

mnesia:transaction(fun() -> mnesia:abort(no_user) end).
% {aborted,no_user}

mnesia:transaction(fun() -> mnesia:delete({usr, 700000003}) end).
% {atomic,ok}

mnesia:transaction(fun() -> mnesia:read({usr, 700000003}) end).
% {atomic, []}


%% # ######## #
%% # Indexing #
%% ########## #

delete_usr(CustId) ->
  F = fun() -> case mnesia:index_read(usr, CustId, id) of
                 []
                   -> {error, instance};
                 [Usr] -> mnesia:delete({usr, Usr#usr.msisdn})
               end
      end,
  {atomic, Result} = mnesia:transaction(F),
  Result.


set_service(CustId, Service, Flag) when Flag==true; Flag==false ->
  F = fun() ->
    case mnesia:index_read(usr, CustId, id) of
      []
        -> {error, instance};
      [Usr] ->
        Services = lists:delete(Service, Usr#usr.services),
        NewServices = case Flag of
                        true -> [Service|Services];
                        false -> Services
                      end,
        mnesia:write(Usr#usr{services=NewServices})
    end
      end,
  {atomic, Result} = mnesia:transaction(F),
  Result.


set_status(CustId, Status) when Status==enabled; Status==disabled->
  F = fun() ->
    case mnesia:index_read(usr, CustId, id) of
      []
        -> {error, instance};
      [Usr] -> mnesia:write(Usr#usr{status=Status})
    end
      end,
  {atomic, Result} = mnesia:transaction(F),
  Result.


% > Add or Remove Indexes During Runtime

add_table_index(Tab, Attribute).
del_table_index(Tab, Attribute).


%% # ################ #
%% # Dirty Operations #
%% ################## #

%% In Mnesia, dirty operations are about 10 times faster than their counterparts that are executed in
%% transactions.
%% If you can guarantee the consistency, isolation, durability, and distribution properties of your
%% tables, dirty operations will significantly enhance the performance of your program.

dirty_read(Oid).
dirty_write(Object).
dirty_delete(ObjectId).
dirty_index_read(Table, SecondaryKey, Attribute).

lookup_id(CustId) ->
  case mnesia:dirty_index_read(usr, CustId, id) of
    [Usr] -> {ok, Usr};
    []
      -> {error, instance}
  end.

%% Service API
lookup_msisdn(PhoneNo) ->
  case mnesia:dirty_read({usr, PhoneNo}) of
    [Usr] -> {ok, Usr};
    []
      -> {error, instance}
  end.
service_flag(PhoneNo, Service) ->
  case lookup_msisdn(PhoneNo) of
    {ok,#usr{services=Services, status=enabled}} ->
      lists:member(Service, Services);
    {ok, #usr{status=disabled}} ->
      {error, disabled};
    {error, Reason} ->
      {error, Reason}
  end.


%% Note:
%% If you need to use dirty operations in a distributed environment, the trick is to ensure
%% that updates to a certain key subset are serialized through a process on a single node.
%% If your keys are in the range of 1 to 1,000, you could potentially update all even keys
%% on one node and all odd ones on the other, solving the race condition we just described.

