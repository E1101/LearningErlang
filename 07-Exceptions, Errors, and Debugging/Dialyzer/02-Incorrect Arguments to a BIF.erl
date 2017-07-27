% --- dialyzer/test2.erl -----------------

-module(test2).

-export([f1/0]).

f1() ->
  tuple_size(list_to_tuple({a,b,c})).


% then

> dialyzer test2.erl
%% test2.erl:4: Function f1/0 has no local return
%% test2.erl:5: The call erlang:list_to_tuple({'a','b','c'})
%%  will never return since it differs in the 1st argument from the
%%  success typing arguments: ([any()])
