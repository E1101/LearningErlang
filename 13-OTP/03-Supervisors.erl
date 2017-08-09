supervisor:start_link(ServerName, CallBackModule, Arguments)
supervisor:start(ServerName, CallBackModule, Arguments)
supervisor:start_link(CallBackModule, Arguments)
supervisor:start(CallBackModule, Arguments)

% return: {ok, {SupervisorSpecification, ChildSpecificationList}}
%% - supervisor specification is a tuple containing information on how to handle process
%%    crashes and restarts.
%% - child specification list specifies which children the supervisor
%% has to start and monitor, together with information on how to terminate and restart them.

%% In the preceding calls:
%% - ServerName
%%   Is the name to be registered for the supervisor, and is a tuple of the format {local, Name}
%%   or {global, Name} . If you do not want to register the supervisor, you use the functions of arity two.
%% - CallbackModule
%%   Is the name of the module in which the init/1 callback function is placed.
%% - Arguments
%%   Is a valid Erlang term that is passed to the init/1 callback function when it is called.


%% # ######################### #
%% # Supervisor Specifications #
%% # ######################### #

% {RestartStrategy, AllowedRestarts, MaxSeconds}

%% > RestartStrategy
%%   - one_for_one:
%%     restart the child that has terminated, without affecting any of the other children.
%%     You should pick this strategy if all of the processes at this level of the super-
%%     vision tree are not dependent on each other.
%%   - one_for_all:
%%     terminate all of the children and restart them. You should use this if there is
%%     a strong dependency among all of the children regardless of the order in which they
%%     were started.
%%   - rest_for_one:
%%     terminate all of the children that were started after the child that crashed, and
%%     will restart them. This strategy assumes that processes are started in order of dependency,
%%     where spawned processes are dependent only on their already started
%%     siblings.
%% > AllowedRestarts comes in, by specifying the maximum number of abnormal terminations
%%   the supervisor is allowed to handle in MaxSeconds seconds.


%% # #################### #
%% # Child Specifications #
%% # #################### #

% {Id, {Module, Function, Arguments}, Restart, Shutdown, Type, ModuleList}

%% > Id
%%   unique identifier for a particular child within a supervisor. As a child process
%%   can crash and be restarted, its process identifier might change.
%% > {Module, Function, Arguments}
%%   to start the child process.
%% > Restart
%%   - transient: never restarted.
%%   - temporary: restarted only if they terminate abnormally.
%%   - permanent: always restarted, regardless of whether the termination was normal or nonnormal.
%% > Shutdown
%% > Type
%%   whether the child process is a worker or a supervisor.


%% # ################ #
%% # Dynamic Children #
%% # ################ #

supervisor:start_child(SupervisorName, ChildSpec)
supervisor:terminate_child(SupervisorName, Id)
supervisor:restart_child(SupervisorName, Id)
supervisor:delete_child(SupervisorName, Id).

%% > SupervisorName
%%   Is either the process identifier of the supervisor or its registered name
%% > ChildSpec
%%   Described up.
%% > Id
%%   Is the unique child identifier defined in the ChildSpec

