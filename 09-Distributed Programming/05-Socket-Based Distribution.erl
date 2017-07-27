%% # ################################### #
%% # Controlling Processes with lib_chan #
%% # ################################### #


% > configuration file.

{port, 1234}.
{service, nameServer, password, "ABXy45", mfa, mod_name_server, start_me_up, notUsed}.

%% This means we are going to offer a service called nameServer on port 1234 of
%% our machine. The service is protected by the password ABXy45 .


% > When a connection is created by the client calling the following:

connect(Host, 1234, nameServer, "ABXy45", nil).

%% the server will spawn mod_name_server:start_me_up(MM, nil, notUsed) . MM is the PID of
%% a proxy process that is used to talk to the client.


% --- socket_dist/mod_name_server.erl ---------------------------------------------

-module(mod_name_server).
-export([start_me_up/3]).

start_me_up(MM, _ArgsC, _ArgS) ->
  loop(MM).

loop(MM) ->
  receive
    {chan, MM, {store, K, V}} ->
      kvs:store(K, V),
      loop(MM);
    {chan, MM, {lookup, K}} ->
      MM ! {send, kvs:lookup(K)},
      loop(MM);
    {chan_closed, MM} ->
      true
  end.



% Now we can start the name server (and the module kvs ).

kvs:start().
lib_chan:start_server().
% Starting a port server on 1234...

% Now we can start a second Erlang session and test this from any client.

{ok, Pid} = lib_chan:connect("localhost",1234,nameServer,"ABXy45","").
lib_chan:cast(Pid, {store, joe, "writing a book"}).
% {send,{store,joe,"writing a book"}}
lib_chan:rpc(Pid, {lookup, joe}).
% {ok,"writing a book"}
lib_chan:rpc(Pid, {lookup, jim}).
% undefined

