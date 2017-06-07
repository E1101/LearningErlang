%% # ############################### #
%% # A Generic Event Manager Example #
%% # ############################### #

% client functions:

start(Name, HandlerList)
%% Will start a generic event manager, registering it with the alias Name . HandlerList is
%- a list of tuples of the form {Handler, Data} , where Handler is the name of the handler
%- callback module and Data is the argument passed to the handler’s init callback
%- function. HandlerList can be empty at startup, as handlers can be subsequently
%- added using the add_handler/2 call.

stop(Name)
%% Will terminate all the handlers and stop the event manager process. It will return
%- a list of items of the form {Handler, Data} , where Data is the return value of the
%- terminate callback function of the individual handlers.

add_handler(Name, Handler, Data)
%% Will add the handler defined in the callback module Handler , passing Data as an
%- argument to the handler’s init callback function.

delete_handler(Name, Handler)
%% Will remove the handler defined in the callback module Handler . The handler’s
%- terminate callback function will be called, and its return value will be the return
%- value of this call. This call returns the tuple {error, instance} if Handler does not
%- exist.

get_data(Name, Handler)
%% Will return the contents of the state variable of the Handler . This call returns the
%- tuple {error, instance} if Handler does not exist.

send_event(Name, Event)
%% Will forward the contents of Event to all the handlers.

add_handler(Name, Handler, InitData) ->
  call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) ->
  call(Name, {delete_handler, Handler}).

get_data(Name, Handler) ->
  call(Name, {get_data, Handler}).

send_event(Name, Event) ->
  call(Name, {send_event, Event}).

handle_msg({add_handler, Handler, InitData}, LoopData) ->
  {ok, [{Handler, Handler:init(InitData)}|LoopData]};

handle_msg({delete_handler, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false ->
      {{error, instance}, LoopData};
    {value, {Handler, Data}} ->
      Reply = {data, Handler:terminate(Data)},
      NewLoopData = lists:keydelete(Handler, 1, LoopData),
      {Reply, NewLoopData}
  end;

handle_msg({get_data, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1, LoopData) of
      false -> {{error, instance}, LoopData};
      {value, {Handler, Data}} -> {{data, Data}, LoopData}
end;

handle_msg({send_event, Event}, LoopData) ->
  {ok, event(Event, LoopData)}.

event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
  [{Handler, Handler:handle_event(Event, Data)}|event(Event, Rest)].

call(Name, Msg) ->
  Name ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
  To ! {reply, Msg}.

loop(State) ->
  receive
    {request, From, Msg} ->
      {Reply, NewState} = handle_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From} ->
      reply(From, terminate(State))
  end.


%% --

-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([init/1]).

start(Name, HandlerList) ->
  register(Name, spawn(event_manager, init, [HandlerList])), ok.

init(HandlerList) ->
  loop(initialize(HandlerList)).

initialize([]) -> [];
initialize([{Handler, InitData}|Rest]) ->
  [{Handler, Handler:init(InitData)}|initialize(Rest)].

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([]) -> [];
terminate([{Handler, Data}|Rest]) ->
  [{Handler, Handler:terminate(Data)}|terminate(Rest)].



-module(io_handler).
-export([init/1, terminate/1, handle_event/2]).

init(Count) -> Count.

terminate(Count) -> {count, Count}.

handle_event({raise_alarm, Id, Alarm}, Count) ->
  print(alarm, Id, Alarm, Count),
  Count+1;

handle_event({clear_alarm, Id, Alarm}, Count) ->
  print(clear, Id, Alarm, Count),
  Count+1;

handle_event(Event, Count) ->
  Count.

print(Type, Id, Alarm, Count) ->
  Date = fmt(date()), Time = fmt(time()),
  io:format("#~w,~s,~s,~w,~w,~p~n",
    [Count, Date, Time, Type, Id, Alarm]).

fmt({AInt,BInt,CInt}) ->
  AStr = pad(integer_to_list(AInt)),
  BStr = pad(integer_to_list(BInt)),
  CStr = pad(integer_to_list(CInt)),
  [AStr,$:,BStr,$:,CStr].

pad([M1]) -> [$0,M1];
pad(Other) -> Other.



-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).

init(File) ->
  {ok, Fd} = file:open(File, write),
  Fd.

terminate(Fd) -> file:close(Fd).

handle_event({Action, Id, Event}, Fd) ->
  {MegaSec, Sec, MicroSec} = now(),
  Args = io:format(Fd, "~w,~w,~w,~w,~w,~p~n",
    [MegaSec, Sec, MicroSec, Action, Id, Event]),
  Fd;

handle_event(_, Fd) ->
  Fd.


%% then:

event_manager:start(alarm, [{log_handler, "AlarmLog"}]).
event_manager:send_event(alarm, {raise_alarm, 10, cabinet_open}).
event_manager:add_handler(alarm, io_handler, 1).
event_manager:send_event(alarm, {clear_alarm, 10, cabinet_open}). % #1,2009:03:16,08:33:14,clear,10,cabinet_open



