-module(add_one).
-export([start/0, request/1, loop/0]).

start() ->
  register(add_one, spawn_link(add_one, loop, [])).

request(Int) ->
  add_one ! {request, self(), Int},
  receive
    {result, Result} -> Result
  after 1000  -> timeout
  end.

loop() ->
  receive
    {request, Pid, Msg} ->
      Pid ! {result, Msg + 1}
  end,
  loop().



-module(add_two).
-export([start/0, request/1, loop/0]).

start() ->
  process_flag(trap_exit, true), %% allowing exit signals to be converted to messages of the format {'EXIT', Pid, Reason}
  Pid = spawn_link(add_two, loop, []),
  register(add_two, Pid),
  {ok, Pid}.

request(Int) ->
  add_two ! {request, self(), Int},
    receive
      {result, Result} -> Result;
      {'EXIT', _Pid, Reason} -> {error, Reason}
      after 1000 -> timeout
    end.

loop() ->
  receive
    {request, Pid, Msg} ->
      Pid ! {result, Msg + 2}
  end,
  loop().

