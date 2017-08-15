%% the callback module for this server is exactly the same as the
%% callback module we used for server1 . By changing the server and keeping the
%% callback module constant, we can change the nonfunctional behavior of the
%% callback module.

%% a server that crashes the client if the query in the server results in an
%% exception

-module(server2).
-export([start/2, rpc/2]).

start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name,Mod,Mod:init()) end)).

rpc(Name, Request) ->
  Name ! {self(), Request},
  receive
    {Name, crash} -> exit(rpc);
    {Name, ok, Response} -> Response
  end.

loop(Name, Mod, OldState) ->
  receive
    {From, Request} ->
      try Mod:handle(Request, OldState) of
        {Response, NewState} ->
          From ! {Name, ok, Response},
          loop(Name, Mod, NewState)
      catch
        %% it loops with original value of State if
        %% an exception was raised in the handler function.
        _:Why ->
          log_the_error(Name, Request, Why),
          %% send a message to cause the client to crash
          From ! {Name, crash},
          %% loop with the *original* state
          %% the state of the server is not changed when an error occurs
          loop(Name, Mod, OldState)
      end
  end.

log_the_error(Name, Request, Why) ->
  io:format("Server ~p request ~p ~n"
  "caused exception ~p~n",
    [Name, Request, Why]).

