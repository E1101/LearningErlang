
% > To open a socket, on both the client and the server side

gen_udp:open(Port)
gen_udp:open(Port, OptionList)
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
%   {ip, ip_address()}: used when opening a socket on a computer that has several network interfaces defined.
%   inet6: set up the socket for IPv6.


% > closes the socket and frees the port number allocated to it

gen_udp:close(Socket)


% > send messages

gen_udp:send(Socket, Address, Port, Packet)


% > When the socket is opened in passive mode
gen_udp:recv(Socket, Length)
gen_udp:recv(Socket, Length, Timeout)

