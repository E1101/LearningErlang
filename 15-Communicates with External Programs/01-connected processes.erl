%
-spec open_port(PortName, [Opt]) -> Port.
%% PortName is one of the following:
%% - {spawn, Command}
%%   Start an external program. Command is the name of an external program.
%%   Command runs outside the Erlang workspace unless a linked-in driver
%%   with the name Command is found.
%% - {fd, In, Out}
%%   Allow an Erlang process to access any currently opened file descriptors
%%   used by Erlang. The file descriptor In can be used for standard input,
%%   and the file descriptor Out can be used for standard output.
%% Opt is one of the following:
%% - {packet, N}
%%   Packets are preceded by an N (1, 2, or 4) byte length count.
%% - stream
%%   Messages are sent without packet lengths. The application must know
%%   how to handle these packets.
%% - {line, Max}
%%   Deliver messages on a one-per line basis. If the line is more than Max
%%   bytes, then it is split at Max bytes.
%% - {cd, Dir}
%%   Valid only for the {spawn, Command} option. The external program starts
%%   in Dir .
%% - {env, Env}
%%   Valid only for the {spawn, Command} option. The environment of the
%%   external program is extended with the environment variables in the
%%   list Env . Env is a list of {VarName, Value} pairs

% > The following messages can be sent to a port:
%   PidC is the PID of the connected process.

Port ! {PidC, {command, Data}}.
% Send Data (an I/O list) to the port.

Port ! {PidC, {connect, Pid1}}.
% Change the PID of the connected process from PidC to Pid1 .

Port ! {PidC, close}.
% Close the port.

% > The connected process can receive a message from the external program by writing this:
receive
  {Port, {data, Data}} ->
  ... Data comes from the external process ...


