%% # ######################################## #
%% # Performing an Action When a Process Dies #
%% # ######################################## #

on_exit(Pid, Fun) ->
  spawn(fun() ->
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Why} ->
        Fun(Why)
    end
        end).

% then:

F = fun() ->
  receive X -> list_to_atom(X) end
end.

Pid = spawn(F).

lib_misc:on_exit(Pid,
  fun(Why) -> io:format(" ~p died with:~p~n",[Pid, Why]) end
).


%% # ############################################### #
%% # Making a Set of Processes That All Die Together #
%% # ############################################### #

%% Suppose we want to create several worker processes that are used to solve
%% some problem. They evaluate the functions F1 , F2 , and so on. If any process
%% dies, we want them all to die. We can do this by calling start([F1,F2, ...]) .

start(Fs) ->
  spawn(fun() ->
    [spawn_link(F) || F <- Fs],
    receive
    after
      infinity -> true
    end
  end
  ).

%% If we want to know whether the processes have all died, we can add an on_exit
%% handler to the start process.

Pid = start([F1, F2, ...]),
on_exit(Pid, fun(Why) ->
... the code here runs if any worker
... process dies
end)


%% # ################################ #
%% # Making a Process That Never Dies #
%% # ################################ #
%% if it dies for any reason, it will be immediately restarted.

keep_alive(Name, Fun) ->
  register(Name, Pid = spawn(Fun)),
  on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

