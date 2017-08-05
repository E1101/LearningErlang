% > A Simple TCP Server

start_nano_server() ->
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
    {reuseaddr, true},
    {active, true}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  gen_tcp:close(Listen),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n",[Bin]),
      Str = binary_to_term(Bin),
      io:format("Server (unpacked) ~p~n",[Str]),
      Reply = lib_misc:string2value(Str),
      io:format("Server replying = ~p~n",[Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.


% > A Sequential Server:
%   To make a sequential server, we change this code to the following:

start_seq_server() ->
  {ok, Listen} = gen_tcp:listen(...),
  seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket),
  seq_loop(Listen).

loop(..) -> %% as before



% > A Parallel Server:
%%  The trick to making a parallel server is to immediately spawn a new process
%%  each time gen_tcp:accept gets a new connection.

start_parallel_server() ->
  {ok, Listen} = gen_tcp:listen(...),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(..) -> %% as before

%% Our parallel server can potentially create many thousands of connections.
%% We might want to limit the maximum number of simultaneous connec-
%% tions. This can be done by maintaining a counter of how many connections
%% are alive at any one time. We increment this counter every time we get a
%% new connection, and we decrement the counter each time a connection
%% finishes. We can use this to limit the total number of simultaneous con-
%% nections in the system.

%% After we have accepted a connection, it’s a good idea to explicitly set the
%% required socket options, like this:
%% {ok, Socket} = gen_tcp:accept(Listen),
%% inet:setopts(Socket, [{packet,4},binary,
%% {nodelay,true},{active, true}]),
%% loop(Socket)

