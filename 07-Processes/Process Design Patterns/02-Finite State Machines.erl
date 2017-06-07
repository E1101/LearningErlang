idle() ->
  receive
    {Number, incoming} ->
      start_ringing(),
      ringing(Number);
    off_hook ->
      start_tone(),
      dial()
  end.

ringing(Number) ->
  receive
    {Number, other_on_hook} ->
      stop_ringing(),
      idle();
    {Number, off_hook} ->
      stop_ringing(),
      connected(Number)
  end.

start_ringing() -> ...
start_tone() -> ...
stop_ringing() -> ...


%% # ################# #
%% # A Mutex Semaphore #
%% # ################# #

%% ! semaphore is a process that serializes access to a particular resource,
%-  guaranteeing mutual exclusion.
%% Assume that only one process at a time is allowed to use the file server, thus guaran-
%% teeing that no two processes are simultaneously reading or writing to the same file.

  -module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.

wait() ->
  mutex ! {wait, self()},
  receive ok -> ok end.

signal() ->
  mutex ! {signal, self()}, ok.

init() ->
  free().

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

busy(Pid) ->
  receive
    {signal, Pid} ->
      free()
  end.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.
