%% Erlang sockets can be opened in one of three modes: active, active once, or
%% passive. This is done by including an option {active, true | false | once} in the Options
%% argument to either gen_tcp:connect(Address, Port, Options or gen_tcp:listen(Port, Options) .

%% The difference between active and passive sockets has to do with what happens
%% when messages are received by the socket.

%% • Once an active socket has been created, the controlling process will be
%%   sent {tcp, Socket, Data} messages as data is received. There is no way the
%%   controlling process can control the flow of these messages. A rogue client
%%   could send thousands of messages to the system, and these would all be
%%   sent to the controlling process. The controlling process cannot stop this
%%   flow of messages.
%% • If the socket was opened in passive mode, then the controlling process
%%   has to call gen_tcp:recv(Socket, N) to receive data from the socket. It will then
%%   try to receive exactly N bytes from the socket. If N = 0 , then all available
%%   bytes are returned. In this case, the server can control the flow of messages
%%   from the client by choosing when to call gen_tcp:recv .


% > Active Message Reception (Nonblocking)

{ok, Listen} = gen_tcp:listen(Port, [..,{active, true}...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).
loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      ... do something with the data ...
      {tcp_closed, Socket} ->
...
end.

%% If the client produces data faster than the server can consume this data, then the
%% system can be flooded with messages—the message buffers will fill up, and
%% the system might crash or behave strangely.
%% This type of server is called a nonblocking server because it cannot block the client.


% > Passive Message Reception (Blocking)

{ok, Listen} = gen_tcp:listen(Port, [..,{active, false}...]),
{ok, Socket} = gen_tcp:accept(Listen),

loop(Socket).

loop(Socket) ->
  %% The client will block until the server has called recv .
  case gen_tcp:recv(Socket, N) of
    {ok, B} ->
      ... do something with the data ...
      loop(Socket);
    {error, closed}
    ...
  end.


% > The Hybrid Approach (Partial Blocking)

%%  when we’re in passive mode, we can wait for the
%%  data from only one socket. This is useless for writing servers that must wait
%%  for data from multiple sockets.

{ok, Listen} = gen_tcp:listen(Port, [..,{active, once}...]),
{ok, Socket} = gen_tcp:accept(Listen),

loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      ... do something with the data ...
      %% when you're ready enable the next message
      inet:setopts(Sock, [{active, once}]),
      loop(Socket);
    {tcp_closed, Socket} ->
      ...
end.

