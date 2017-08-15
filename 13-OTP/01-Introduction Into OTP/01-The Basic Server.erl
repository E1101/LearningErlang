%% stop and think. The *callback* had no code for concurrency, no spawn, no
%% send, no receive, and no register. It is pure sequential code—nothing else.
%% This means we can write client-server models without understanding anything
%% about the underlying concurrency models.

-module(server1).
-export([start/2, rpc/2]).

start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
  Name ! {self(), Request},
  receive
    {Name, Response} -> Response
  end.

loop(Name, Mod, State) ->
  receive
    {From, Request} ->
      {Response, State1} = Mod:handle(Request, State),
        From ! {Name, Response},
        loop(Name, Mod, State1)
  end.


%% Let’s write a *callback* for server:

-module(name_server).
-export([init/0, add/2, find/1, handle/2]).
-import(server1, [rpc/2]).

%% client routines
add(Name, Place) -> rpc(name_server, {add, Name, Place}).
find(Name) -> rpc(name_server, {find, Name}).

%% callback routines
init() -> dict:new().

handle({add, Name, Place}, Dict) -> {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) -> {dict:find(Name, Dict), Dict}.


%% then:

server1:start(name_server, name_server).
% true
name_server:add(joe, "at home").
% ok
name_server:find(joe).
% {ok,"at home"}


