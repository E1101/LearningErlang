%% A supervision tree is a tree of processes. The upper processes (supervisors)
%% in the tree monitor the lower processes (workers) in the tree and restart the
%% lower processes if they fail.

% - One-for-one supervision trees
%   if a worker fails, it is restarted by the supervisor.
%
% - One-for-all supervision trees
%   if any worker dies, then all the worker processes
%   are killed (by calling the terminate/2 function in the appropriate callback
%   module). Then all the worker processes are restarted.


% To do this, we’ll write yet another callback module, this time for gen_supervisor .

-module(sellaprime_supervisor).
-behaviour(supervisor).
% see erl -man supervisor
-export([start/0, start_in_shell_for_testing/0, start_link/1, init/1]).

start() ->
  spawn(fun() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
        end).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).

start_link(Args) ->
  supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
%% Install my personal error handler
  gen_event:swap_handler(alarm_handler,
    {alarm_handler, swap},
    {my_alarm_handler, xyz}),
  {ok, {{one_for_one, 3, 10},
    [{tag1,
      {area_server, start_link, []},
      permanent,
      10000,
      worker,
      [area_server]},
      {tag2,
        {prime_server, start_link, []},
        permanent,
        10000,
        worker,
        [prime_server]}
    ]}}.


% The Worker specifications are tuples of the following form:

% Tag: This is an atom tag that we can use to refer to the worker process later
{Tag, {Mod, Func, ArgList}, % the function that the supervisor will use to start the worker.
  % Restart = permanent | transient | temporary
  Restart,
  % maximum time a worker is allowed
  % to take in terminating. If it takes longer
  % than this, it will be killed.
  Shutdown,
  % Type = worker | supervisor
  % We can construct a tree of
  % supervisors by adding supervisor processes
  % in place of worker processes.
  Type,
  % the name of the callback module if the child process
  % is a supervisor or gen_server behavior callback module
  [Mod1]}

