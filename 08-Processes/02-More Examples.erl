-module(echo).
-export([go/0, loop/0]).

go() ->
  Pid = spawn(echo, loop, []),
  Pid ! {self(), hello},
  receive
    {Pid, Msg} ->
      io:format("~w~n",[Msg])
  end,
  Pid ! stop.

loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop ->
      true % terminates normally
  end.


% > The registered echo process
-module(echo).
-export([go/0, loop/0]).

go() ->
  register(echo, spawn(echo, loop, [])),
  echo ! {self(), hello},
  receive
    {_Pid, Msg} ->
      io:format("~w~n",[Msg])
  end.

loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop ->
      true
  end.


% > Implementing a Timer
-module(stimer).
-export([start/2, cancel/1]).

start(Time, Fun)
  -> spawn(fun() -> timer(Time, Fun) end).
cancel(Pid) -> Pid ! cancel.
timer(Time, Fun) ->
  receive
    cancel ->
      void
  after Time ->
    Fun()
  end.

% then:

Pid1 = stimer:start(25000, fun() -> io:format("timer event~n") end).
stimer:cancel(Pid1).


%% # ############ #
%% # Benchmarking #
%% # ############ #

-module(myring).
-export([start/1, start_proc/2]).

start(Num) ->
  start_proc(Num, self()).

start_proc(0, Pid) ->
  Pid ! ok;

start_proc(Num, Pid) ->
  NPid = spawn(?MODULE, start_proc, [Num-1, Pid]), % ?MODULE module name itself
  NPid ! ok,
  receive ok -> ok end.
% then:
timer:tc(myring, start, [100000]).  % {484000,ok}
timer:tc(myring, start, [1000000]). % {4289360,ok}

