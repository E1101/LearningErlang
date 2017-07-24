%
% ---- fac.erl ---------------------
-module(fac).
-export([fac/1]).

fac(0) -> 1;
fac(N) -> N*fac(N-1).
% ----------------------------------

% ---- fac1.erl --------------------
-module(fac1).
-export([main/1]).

main([A]) ->
  I = list_to_integer(atom_to_list(A)),
  F = fac(I),
  io:format("factorial ~w = ~w~n",[I, F]),
  init:stop().
% -----------------------------------

$ erlc fac1.erl
$ erl -noshell -s fac1 main 25


% > we can run it as an escript.

% ---- factorial ---------------------
#!/usr/bin/env escript

main([A]) ->
  I = list_to_integer(A),
  F = fac(I),
  io:format("factorial ~w = ~w~n",[I, F]).
% ------------------------------------

$ ./factorial 25

