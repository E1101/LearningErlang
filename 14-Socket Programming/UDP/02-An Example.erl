% > A UDP Factorial Server

-module(udp_test).
-export([start_server/0, client/1]).

start_server() ->
  spawn(fun() -> server(4000) end).

%% The server
server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("server opened socket:~p~n",[Socket]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} = Msg ->
      io:format("server received:~p~n",[Msg]),
      N = binary_to_term(Bin),
      Fac = fac(N),
      gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
      loop(Socket)
  end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).


%% The client
client(N) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  io:format("client opened socket=~p~n",[Socket]),
  ok = gen_udp:send(Socket, "localhost", 4000, term_to_binary(N)),
  Value = receive
            {udp, Socket, _, _, Bin} = Msg ->
              io:format("client received:~p~n",[Msg]),
              binary_to_term(Bin)
          after 2000 ->
              0
          end,

  gen_udp:close(Socket),
  Value.

%% ! It might happen that the reply to a second query was in fact a
%%   duplicated answer to the first query.
%%   To avoid this, we could modify the client code to include a unique
%%   reference and check that this reference is returned by the server.

%%   we call the Erlang BIF make_ref , which is guaranteed to
%%   return a globally unique reference.

client(Request) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  Ref = make_ref(), %% make a unique reference
  B1 = term_to_binary({Ref, Request}),
  ok = gen_udp:send(Socket, "localhost", 4000, B1),
  wait_for_ref(Socket, Ref).

wait_for_ref(Socket, Ref) ->
  receive
    {udp, Socket, _, _, Bin} ->
      case binary_to_term(Bin) of
        {Ref, Val} ->
          %% got the correct value
          Val;
        {_SomeOtherRef, _} ->
          %% some other value throw it away
          wait_for_ref(Socket, Ref)
      end;
      after 1000 ->
        ...
  end.

