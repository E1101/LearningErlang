% --- dialyzer/test3.erl ---------------------
-module(test3).

-export([test/0, factorial/1]).

test() -> factorial(-5).

factorial(0) -> 1;
factorial(N) -> N*factorial(N-1).

% then

> dialyzer test3.erl
%% test3.erl:4: Function test/0 has no local return
%% test3.erl:4: The call test3:factorial(-5) will never return since
%%  it differs in the 1st argument from the success typing
%%  arguments: (non_neg_integer())

