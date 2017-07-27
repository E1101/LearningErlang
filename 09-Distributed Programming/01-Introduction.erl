% --- socket_dist/kvs.erl -------------------------------------
-module(kvs).
-export([start/0, store/2, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->
  kvs ! {self(), Q},
  receive
    {kvs, Reply} -> Reply
  end.

loop() ->
  receive
    {From, {store, Key, Value}} ->
      put(Key, {ok, Value}),
      From ! {kvs, true},
      loop();
    {From, {lookup, Key}} ->
      From ! {kvs, get(Key)},
      loop()
end.


%% # ####################################################### #
%% # Client on One Node, Server on Second Node but Same Host #
%% # ####################################################### #

% start an Erlang node with name gandalf on the local host.
$ erl -sname gandalf
(gandalf@localhost) 1> kvs:start().

% on other terminal:
$ erl -sname bilbo
(bilbo@localhost) 1> rpc:call(gandalf@localhost, kvs,store, [weather, fine]).
% true
(bilbo@localhost) 2> rpc:call(gandalf@localhost, kvs,lookup,[weather]).
% {ok,fine}

%% rpc:call(Node, Mod, Func, [Arg1, Arg2, ..., ArgN]) performs a remote procedure
%% call on Node. The function to be called is Mod:Func(Arg1, Arg2, ..., ArgN) .


%% # ####################################################### #
%% # Client and Server on Different Machines on the Same LAN #
%% # ####################################################### #

$ erl -name gandalf -setcookie abc
(gandalf@doris.myerl.example.com) 1> kvs:start().

% on other machine:
$ erl -name bilbo -setcookie abc
(bilbo@george.myerl.example.com) 1> rpc:call(gandalf@doris.myerl.example.com, kvs,store,[weather,cold]).
(bilbo@george.myerl.example.com) 2> rpc:call(gandalf@doris.myerl.example.com, kvs,lookup,[weather]).

%% When we have two nodes on the same machine, we use “short” names
%% (as indicated by the -sname flag), but if they are on different
%% networks, we use -name .

%% We can also use -sname on two different machines when they are on the
%% same subnet. Using -sname is also the only method that will work if no
%% DNS service is available.

%% Ensure that both nodes have the same cookie. This is why both nodes
%% were started with the command-line argument -setcookie abc .

% > To prepare your system for distributed Erlang:
% - Make sure that port 4369 is open for both TCP and UDP traffic. This port
%   is used by a program called epmd (short for the Erlang Port Mapper Daemon).
% - Choose a port or range of ports to be used for distributed Erlang, and
%   make sure these ports are open. (use Min = Max if you want to use only one port)

$ erl -name ... -setcookie ... -kernel inet_dist_listen_min Min \
  inet_dist_listen_max Max

