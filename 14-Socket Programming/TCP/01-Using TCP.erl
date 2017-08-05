% > Fetching Data from a Server

nano_get_url() ->
  nano_get_url("www.google.com").

nano_get_url(Host) ->
  %% The argument binary in the connect call tells the system to
  %% open the socket in “binary” mode and deliver all data to the
  %% application as binaries.
  %% {packet,0} means the TCP data is delivered directly to the
  %% application in an unmodified form.
  {ok,Socket} = gen_tcp:connect(Host,80,[binary, {packet, 0}]),
  ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
  receive_data(Socket, []).

receive_data(Socket, SoFar) ->
  receive
    {tcp,Socket,Bin} ->
      receive_data(Socket, [Bin|SoFar]);
    {tcp_closed,Socket} ->
      list_to_binary(reverse(SoFar))
  end.


% > A Simple TCP Server

start_nano_server() ->
  %% {packet, 4} means that each application message will be preceded by a 4-byte length header.
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
    {reuseaddr, true},
    {active, true}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  %% When accept returns, we immediately call gen_tcp:close(Listen) . This closes
  %% down the listening socket, so the server will not accept any new connec-
  %% tions. This does not affect the existing connection; it just prevents new
  %% connections.
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

% To test the server, we need a corresponding client.

nano_client_eval(Str) ->
  {ok, Socket} =
    gen_tcp:connect("localhost", 2345,
      [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary(Str)),
  receive
    {tcp,Socket,Bin} ->
      io:format("Client received binary = ~p~n",[Bin]),
      Val = binary_to_term(Bin),
      io:format("Client result = ~p~n",[Val]),
      gen_tcp:close(Socket)
  end.


