-module(broadcast).
-compile(export_all).

send(IoList) ->
  case inet:ifget("eth0", [broadaddr]) of
    {ok, [{broadaddr, Ip}]} ->
      {ok, S} = gen_udp:open(5010, [{broadcast, true}]),
      gen_udp:send(S, Ip, 6000, IoList),
      gen_udp:close(S);
    _ ->
      io:format("Bad interface name, or\n"
      "broadcasting not supported\n")
  end.

listen() ->
  {ok, _} = gen_udp:open(6000),
  loop().

loop() ->
  receive
    Any ->
      io:format("received:~p~n", [Any]),
      loop()
  end.

%% Only the process performing a broadcast opens port 5010, but all machines
%% in the network call broadcast:listen() , which opens port 6000 and listens for
%% broadcast messages.

%% ! Note also that if hosts running UDP listeners are on different
%%   network subnets, the UDP broadcasts are unlikely to reach them, because
%%   by default routers drop such UDP broadcasts.

