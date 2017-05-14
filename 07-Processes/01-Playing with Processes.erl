% Erlang’s key organizational concept is the process, an independent
% component (built from functions) that sends and receives messages.

% > The first thing to explore is the process identifier, often called a pid.
% - The easiest pid to get is your own

self(). % <0.36.0>

%% !! you can’t type pids directly into the shell or into functions.

%% !! Erlang, however, will never report that a message send failed, even if the pid doesn’t
%% -- point to a real process. It also won’t report that a message was ignored by a process.

% > send messages to process
Pid=self().
Pid ! test2.


flush().

% Shell got test1
% Shell got test2
%% then remove all received messages.


% > read received messages
self() ! test1.
%% waiting for something to arrive in the mailbox
receive X -> X end. % test1
%% !! hit Ctrl-G, and then type q . You’ll have to restart Erlang.

self() ! 23.
receive Y->2*Y end. % 46

