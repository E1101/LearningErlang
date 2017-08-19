%% # ############################ #
%% # Write the Interface Routines #
%% # ############################ #

start().
% Open the bank.

stop().
% Close the bank.

new_account(Who).
% Create a new account.

deposit(Who, Amount).
% Put money in the bank.

withdraw(Who, Amount).
% Take money out, if in credit.


%% Each of these results in exactly one call to the routines in gen_server , as follows:

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
% starts a local server, If the first argument
% was the atom global , it would start a global server that could be accessed on
% a cluster of Erlang nodes.

stop() -> gen_server:call(?MODULE, stop).
new_account(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).


%% # ########################### #
%% # Write the Callback Routines #
%% # ########################### #

%% ---- gen_server_template.mini --------------------------------------------
-module().
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% by calling gen_server:start_link first routine
%% to be called in the callback module is Mod:init(StartArgs)
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) -> {ok, State}.

handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.


%% ---- my_bank.erl ------------------------------------------------------------

init([]) -> {ok, ets:new(?MODULE,[])}.

handle_call({new,Who}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> ets:insert(Tab, {Who,0}),
              {welcome, Who};
            [_] -> {Who, you_already_are_a_customer}
          end,
  {reply, Reply, Tab};

handle_call({add,Who,X}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> not_a_customer;
            [{Who,Balance}] ->
              NewBalance = Balance + X,
              ets:insert(Tab, {Who, NewBalance}),
              {thanks, Who, your_balance_is, NewBalance}
          end,
  {reply, Reply, Tab};

handle_call({remove,Who, X}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> not_a_customer;
            [{Who,Balance}] when X =< Balance ->
              NewBalance = Balance - X,
              ets:insert(Tab, {Who, NewBalance}),
              {thanks, Who, your_balance_is, NewBalance};
            [{Who,Balance}] ->
              {sorry,Who,you_only_have,Balance,in_the_bank}
          end,
  {reply, Reply, Tab};

%% The second argument ( normal ) is used as the first
%% argument to my_bank:terminate/2 .
%% The third argument ( stopped ) becomes the return
%% value of my_bank:stop() .
handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% then:

my_bank:start().
% {ok,<0.33.0>}
my_bank:deposit("joe", 10).
% not_a_customer
my_bank:new_account("joe").
% {welcome,"joe"}
my_bank:deposit("joe", 10).
% {thanks,"joe",your_balance_is,10}
my_bank:deposit("joe", 30).
% {thanks,"joe",your_balance_is,40}
my_bank:withdraw("joe", 15).
% {thanks,"joe",your_balance_is,25}
my_bank:withdraw("joe", 45).
% {sorry,"joe",you_only_have,25,in_the_bank}





