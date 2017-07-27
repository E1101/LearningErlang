%% Message passing is often hidden in functional interfaces,so instead of
%- calling:
%-   printerserver ! {print, File}
%- a client would call:
%-   printerserver:print(File)

%% If the client does not need a reply, the call to the server can be asynchronous.
%- return atom ok

%% Synchronous call when we expect return result,{ok, Result} , or {error, Reason}.

-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.
start() ->
  register(frequency, spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop
loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      reply(Pid, ok),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.


%% The client Functions
stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).
%% We hide all message passing and the message
%% protocol in a functional interface.
call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.



%% # ######################### #
%% # A Process Patter Skeleton #
%% # ######################### #

-module(server).
-export([start/2, stop/1, call/2]).
-export([init/1]).

start(Name, Data) ->
  Pid = spawn(generic_handler, init, [Data]),
  register(Name, Pid), ok.

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

call(Name, Msg) ->
  Name ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
  To ! {reply, Msg}.

init(Data) ->
  loop(initialize(Data)).

loop(State) ->
  receive
    {request, From, Msg} ->
      {Reply,NewState} = handle_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From} ->
      reply(From, terminate(State))
  end.

initialize(...) -> ...
handle_msg(...,...) -> ...
terminate(...) -> ...



%% Let’s rewrite the server, making it reliable by monitoring the clients. When a client is
%% allocated a frequency, the server links to it. If a client terminates before deallocating a
%% frequency, the server will receive an exit signal and deallocate it automatically. If the
%% client does not terminate, and deallocates the frequency using the client function, the
%% server removes the link.

-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.
start() ->
  register(frequency, spawn(frequency, init, [])).

init() ->
  process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The client Functions
stop()
  -> call(stop).

allocate()
  -> call(allocate).

deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.
call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Message) ->
  Pid ! {reply, Message}.

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies=deallocate(Frequencies, Freq),
      reply(Pid, ok),
      loop(NewFrequencies);
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequencies}};

allocate({[Freq|Frequencies], Allocated}, Pid) ->
  link(Pid),
  {{Frequencies,[{Freq,Pid}|Allocated]},{ok,Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),
  unlink(Pid),
  NewAllocated=lists:keydelete(Freq,1,Allocated),
  {[Freq|Free], NewAllocated}.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid,2,Allocated) of
    {value,{Freq,Pid}} ->
      NewAllocated = lists:keydelete(Freq,1,Allocated),
      {[Freq|Free],NewAllocated};
    false ->
      {Free,Allocated}
  end.