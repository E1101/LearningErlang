gen_tcp:listen(PortNumber, Options)
% {ok, Socket} | {error, Reason}

% - Port
%   an integer denoting the listening port number of the socket.
% - OptionList
%   list: Forwards all messages in the packet as a list of integers *default
%   binary: Forwards all messages in the packet as a binary.
%   {header, Size}: used if packets are being received as binaries. It splits the message into a
%                   list of size Size , the header, and the message (a binary).
%   {active, true}: ensures that all the messages received from the socket are
%                   forwarded to the process that owns the socket as Erlang messages of the form
%                   {udp, Socket, IP, PortNo, Packet}. *default
%   {active, false}: Sets the socket to passive mode. Instead of being sent, messages from the socket
%                    have to be retrieved using the gen_udp:recv/2 and gen_udp:recv/3 calls.
%   {active, once}: send the first message it receives to the socket, but subsequent messages have
%                   to be retrieved using the recv functions.
%   {nodelay, true}: result in the socket immediately sending the package, no matter how small.
%   {keepalive, true}: connected socket sends keepalive messages when no data is being
%                      transferred. As “close socket” messages can be lost, this option ensures that the
%                      socket is closed if no response to the keepalive is received.
%   {packet_size, Integer}: the maximum allowed length of the body.
%   {ip, ip_address()}: used when opening a socket on a computer that has several network interfaces defined.
%   inet6: set up the socket for IPv6.




client(Host, Data) ->
  {ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
  send(Socket, Data),
  ok = gen_tcp:close(Socket).


% bind the first 100 bytes of the binary to Chunk and what remains to Rest
send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
  gen_tcp:send(Socket, Chunk),
  send(Socket, Rest);
send(Socket, Rest) ->
  gen_tcp:send(Socket, Rest).



server() ->
  {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
  wait_connect(ListenSocket,0).
wait_connect(ListenSocket, Count) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, wait_connect, [ListenSocket, Count+1]),
  get_request(Socket, [], Count).
get_request(Socket, BinaryList, Count) ->
  case gen_tcp:recv(Socket, 0, 5000) of
    {ok, Binary} ->
      get_request(Socket, [Binary|BinaryList], Count);
    {error, closed} ->
      handle(lists:reverse(BinaryList), Count)
  end.
handle(Binary, Count) ->
  {ok, Fd} = file:open("log_file_"++integer_to_list(Count), write),
  file:write(Fd, Binary),
  file:close(Fd).

