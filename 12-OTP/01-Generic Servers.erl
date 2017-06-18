%% # #################### #
%% # Starting Your Server #
%% # #################### #


% Generic Servers implement client/server behaviors.
% With the gen_server behavior, instead of using the spawn and spawn_link BIFs,
% you will use the gen_server:start/4 and gen_server:start_link/4 functions.

% ! prevents unforeseen race conditions, as the call will not return
%   the pid of the worker until it has been initialized.
gen_server:start_link(ServerName, CallBackModule, Arguments, Options)
gen_server:start(ServerName, CallBackModule, Arguments, Options)

gen_server:start_link(CallBackModule, Arguments, Options)
gen_server:start(CallBackModule, Arguments, Options)

% - ServerName
%   Is a tuple of the format {local, Name} or {global, Name} , denoting a local or global
%   Name for the process if it is to be registered. If you do not want to register the process
%   and instead reference it using its pid, you omit the argument and use the
%   start_link/3 or start/3 call instead.

%% You start a gen_server behavior using the gen_server:start_link call. This results
%% in a new process that calls the init/1 callback function. This function initializes the
%% LoopData and returns the tuple {ok, LoopData} .

start_link(FileName) ->
gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

init(FileName) ->
  usr_db:create_tables(FileName),
  usr_db:restore_backup(),
  {ok, null}. % We don’t really need the LoopData variable in our server


%% # ################ #
%% # Passing Messages #
%% # ################ #

% > send a message to your server
gen_server:cast(Name, Message)
gen_server:call(Name, Message)

% - Name
% Is either the local registered name of the server or the tuple {global, Name} .
% It could also be the process identifier of the server.

gen_server:cast % asynchronous message requests
%% then:
handle_cast(Message, LoopData) % in the callback module will call by gen_server
% {noreply, NewLoopData}
% {stop, Reason, NewLoopData}

gen_server:call % send a synchronous message to the server
%% then:
handle_call(Message, From, LoopData) % in the callback module will call by gen_server
% {reply, Reply, NewLoopData}
% {stop, Reason, Reply, NewLoopData}.

gen_server:call(Name, Message, Timeout) % default is five second, u can use atom infinity


% ..

set_status(CustId, Status) when Status==enabled; Status==disabled ->
  gen_server:call(?MODULE, {set_status, CustId, Status}).

delete_disabled() ->
  gen_server:call(?MODULE, delete_disabled).

% module:

handle_call({set_status, CustId, Status}, _From, LoopData) ->
  Reply = case usr_db:lookup_id(CustId) of
            {ok, Usr} ->
              usr_db:update_usr(Usr#usr{status=Status});
            {error, instance} ->
              {error, instance}
          end,
  {reply, Reply, LoopData};

handle_call(delete_disabled, _From, LoopData) ->
  {reply, usr_db:delete_disabled(), LoopData}.


% - Non-OTP-compliant messages
handle_info/2 % is called whenever the process receives a message it doesn’t recognize.
% {noreply, NewLoopData}
% {stop, Reason, NewLoopData}

handle_info(_Msg, LoopData) ->
  {noreply, LoopData}.



%% # ################### #
%% # Stopping the Server #
%% # ################### #

% In your handle_call/3 and handle_cast/2 callback functions
% return {stop, Reason, Reply, NewLoopData} or {stop, Reason, NewLoopData}
%% then upon receiving:
% the generic code executes the terminate(Reason, LoopData) callback.

% ! stop call does not have to occur within a synchronous call

stop() ->
  gen_server:cast(?MODULE, stop).
handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}. % Any reason other than normal will result in an error report being generated.
terminate(_Reason, _LoopData) ->
  usr_db:close_tables().

