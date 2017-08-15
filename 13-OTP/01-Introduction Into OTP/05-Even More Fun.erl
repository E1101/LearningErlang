%% server that does nothing at all until you tell it to become
%% a particular type of server.

-module(server5).
-export([start/0, rpc/2]).

start() -> spawn(fun() -> wait() end).

wait() ->
  receive
    {become, F} -> F()
  end.

rpc(Pid, Q) ->
  Pid ! {self(), Q},
  receive
    {Pid, Reply} -> Reply
  end.


% Let’s now define a server function.

-module(my_fac_server).
-export([loop/0]).

loop() ->
  receive
    {From, {fac, N}} ->
      From ! {self(), fac(N)},
      loop();
    {become, Something} ->
      Something()
  end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

% then

Pid = server5:start().
Pid ! {become, fun my_fac_server:loop/0}.
server5:rpc(Pid, {fac,30}).
