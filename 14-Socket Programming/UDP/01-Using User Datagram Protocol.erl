%% Using UDP, machines on the Internet can send each
%% other short messages called datagrams.

%% UDP is a connectionless protocol, which means the client does not have to
%% establish a connection to the server before sending it a message. This means
%% that UDP is well suited for applications where large numbers of clients send
%% small messages to a server.


% > The Simplest UDP Server and Client

server(Port) ->
  %% tells the driver to send all messages to the controlling process as binary data.
  {ok, Socket} = gen_udp:open(Port, [binary]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} ->
      BinReply = ... ,
      gen_udp:send(Socket, Host, Port, BinReply),
      loop(Socket)
end.

client(Request) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
   ok = gen_udp:send(Socket, "localhost", 4000, Request),
  Value = receive
            {udp, Socket, _, _, Bin} ->
              {ok, Bin}
          %% We must have a timeout since UDP is unreliable and we might not actually get a reply.
          after 2000 ->
            error
          end,

  gen_udp:close(Socket),
  Value.

