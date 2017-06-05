% Erlang’s key organizational concept is the process, an independent
% component (built from functions) that sends and receives messages.

% > The first thing to explore is the process identifier, often called a pid.
% - The easiest pid to get is your own

self(). % <0.36.0>

%% !! you can’t type pids directly into the shell or into functions.

%% !! Erlang, however, will never report that a message send failed, even if the pid doesn’t
%% -- point to a real process. It also won’t report that a message was ignored by a process.

% > send messages to process
Pid=self(). % <0.30.0>
Pid ! test2.

Pid2 = pid(0,30,0).
Pid2 ! hello2.

flush(). % retrieve and display all the messages sent to the shell process
% Shell got test2
% Shell got hello2
%% then remove all received messages.


%% # ################## #
%% # Creating Processes #
%% # ################## #

spawn(Module , Function , Arguments). % returns a process identifier

% > so if you want to spawn the function m:f/1 with the argument a , you need to call:
spawn(m, f, [a]).

% > send the same message to many processes
Pid1!Msg,Pid2!Msg,Pid3!Msg.
%% or
Pid3!Pid2!Pid1!Message.
%% equivalent to
Pid3!(Pid2!(Pid1!Message)).


%% # ################## #
%% # Receiving Messages #
%% # ################## #

receive
  Pattern1 when Guard1 -> exp11, .., exp1n;
  Pattern2 when Guard2 -> exp21, .., exp2n;
  ...
  Other -> expn1, .., expnn
after
  Timeout -> exp1, .., expn % Timeout is an integer denoting the time in milliseconds
end

% > read received messages
self() ! test1.
%% waiting for something to arrive in the mailbox
receive X -> X end. % test1
%% !! hit Ctrl-G, and then type q . You’ll have to restart Erlang.

self() ! 23.
receive Y->2*Y end. % 46

