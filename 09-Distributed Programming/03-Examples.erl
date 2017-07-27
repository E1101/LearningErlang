%% # ############################# #
%% # An Example of Remote Spawning #
%% # ############################# #

% --- dist_demo.erl --------------------------------------
-module(dist_demo).
-export([rpc/4, start/1]).

start(Node) ->
  spawn(Node, fun() -> loop() end).

rpc(Pid, M, F, A) ->
  Pid ! {rpc, self(), M, F, A},
  receive
    {Pid, Response} ->
      Response
  end.

loop() ->
  receive
    {rpc, Pid, M, F, A} ->
      Pid ! {self(), (catch apply(M, F, A))},
      loop()
  end.

% then:
doris $ erl -name gandalf -setcookie abc
(gandalf@doris.myerl.example.com) 1>

george $ erl -name bilbo -setcookie abc
(bilbo@george.myerl.example.com) 1>

(bilbo@george.myerl.example.com) 1> Pid = dist_demo:start('gandalf@doris.myerl.example.com').
%% evaluates erlang:node() on the remote node and returns the value.
(bilbo@george.myerl.example.com) 2> dist_demo:rpc(Pid, erlang, node, []).
% 'gandalf@doris.myerl.example.com'

