%
-spec spawn_link(Fun) -> Pid.
-spec spawn_link(Mod, Fnc, Args) -> Pid.
%% This behaves like spawn(Fun) or spawn(Mod,Func,Args) and also creates a link
%% between the parent and child processes.

-spec spawn_monitor(Fun) -> {Pid, Ref}.
-spec spawn_monitor(Mod, Func, Args) -> {Pid, Ref}.
%% This is like spawn_link , but it creates a monitor rather than a link. Pid is the
%% process identifier of the newly created process, and Ref is a reference to
%% the process. If the process dies with the reason Why , then the message
%% {'DOWN',Ref,process,Pid,Why} will be sent to the parent process.

-spec process_flag(trap_exit, true).
%% This turns the current process into a system process. A system process
%% is a process that can receive and process error signals.

-spec link(Pid) -> true.
%% This creates a link to the process Pid . Links are symmetric. If a process A
%% evaluates link(B) , then it will be linked to B . The net effect is the same as
%% if B had evaluated link(A) .
%% If the process Pid does not exist, then an exit noproc exception is raised.
%% If A is already linked to B and evaluates link(B) (or vice versa), the call is
%% ignored.

-spec unlink(Pid) -> true.
%% This removes any link between the current process and the process Pid .

-spec erlang:monitor(process, Item) -> Ref.
%% This sets up a monitor. Item is a Pid or a registered name of a process.

-spec demonitor(Ref) -> true.
%% This removes a monitor with reference Ref.

-spec exit(Why) -> none().
%% This causes the current process to terminate with the reason Why . If the
%% clause that executes this statement is not within the scope of a catch
%% statement, then the current process will broadcast an exit signal, with
%% argument Why to all processes to which it is currently linked. It will also
%% broadcast a DOWN message to all processes that are monitoring it.

-spec exit(Pid, Why) -> true.
%% This sends an exit signal with the reason Why to the process Pid . The pro-
%% cess executing this BIF does not itself die. This can be used to “fake” exit
%% signals.

