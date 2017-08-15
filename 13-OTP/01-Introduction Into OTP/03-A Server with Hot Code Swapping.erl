%% Most servers execute a fixed program, and
%% if you want to modify the behavior of the server, you have to stop the server
%% and then restart it with the modified code. When we want to change the
%% behavior of our server, we don’t stop it; we just send it a message containing
%% the new code, and it picks up the new code and continues with the new code
%% and the old session data. This process is called hot code swapping.

-module(server3).
-export([start/2, rpc/2, swap_code/2]).

start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name,Mod,Mod:init()) end)).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
  Name ! {self(), Request},
  receive
    {Name, Response} -> Response
  end.

loop(Name, Mod, OldState) ->
  receive
    %% If we send the server a swap code message, it changes the callback module
    %% to the new module contained in the message.
    {From, {swap_code, NewCallBackMod}} ->
      From ! {Name, ack},
      loop(Name, NewCallBackMod, OldState);

    {From, Request} ->
      {Response, NewState} = Mod:handle(Request, OldState),
      From ! {Name, Response},
      loop(Name, Mod, NewState)
  end.
