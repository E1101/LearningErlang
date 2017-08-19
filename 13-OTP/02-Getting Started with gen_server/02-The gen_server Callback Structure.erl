%% # ################### #
%% # Starting the Server #
%% # ################### #

gen_server:start_link(Name, Mod, InitArgs, Opts).
% The generic server starts by calling
% gen_server:start_link(Name, Mod, InitArgs, Opts). >>> Mod:init(InitArgs) .

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  %% {ok, State} is returned, then we have successfully started the server.
  {ok, #state{}}.


%% # ################## #
%% # Calling the Server #
%% # ################## #

gen_server:call(Name, Request).
% To call the server, the client program calls
% gen_server:call(Name, Request). >>> handle_call/3 in the callback module being called.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                               {reply, Reply, State} |
%%                               {reply, Reply, State, Timeout} |
%%                               {noreply, State} |
%%                               {noreply, State, Timeout} |
%%                               {stop, Reason, Reply, State} |
%%                               {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  %% Reply goes back to the client, where it becomes the return
  %% value of gen_server:call . NewState is the next state of the server.
  %% Calling stop with the appropriate arguments will stop the server.
  {reply, Reply, State}.

%% (the second argument of gen_server:call/2) reappears
%% as the first argument of handle_call/3 .
%% From is the PID of the requesting client process, and State is the
%% current state of the client.


%% # ############### #
%% # Calls and Casts #
%% # ############### #

gen_server:cast(Name, Msg)
%% implements a cast, which is just a call with no return value
%% (actually just a message, but traditionally it’s called a cast to
%% distinguish it from a remote procedure call).

%% gen_server:cast(Name, Msg). >>> handle_cast/2

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  % usually just returns {noreply, NewState}
  % {stop, ...}
  {noreply, State}.



%% # ################################## #
%% # Spontaneous Messages to the Server #
%% # ################################## #

%% Spontaneous messages are any messages that arrive
%% at the server that were not sent by explicitly calling
%% gen_server:call or gen_server:cast .

%% For example, if the server is linked to another process and is trapping exits,
%% then it might suddenly receive a unexpected {'EXIT', Pid, What} message. Alterna-
%% tively, any process in the system that discovers the PID of the generic server
%% can just send it a message. Any message like this ends up at the server as
%% the value of Info .

%% >>> handle_info(Info, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  %% The return values are the same as for handle_cast
  {noreply, State}.


%% # ########### #
%% # Termination #
%% # ########### #

%% One of the handle_Something routines
%% might return a {stop, Reason, NewState} , or the server might crash with {'EXIT',
%% reason} . In all of these circumstances, no matter how they occurred,
%% terminate(Reason, NewState) will be called.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the <mod>gen_server</mod> terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%% If you want your server to be restarted in
%% the future, you’ll have to write an “I’ll be back” function
%% that is triggered by terminate/2 .


%% # ########### #
%% # Code Change #
%% # ########### #

%% You can dynamically change the state of your server while it is running. This
%% callback function is called by the release handling subsystem when the system
%% performs a software upgrade.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

