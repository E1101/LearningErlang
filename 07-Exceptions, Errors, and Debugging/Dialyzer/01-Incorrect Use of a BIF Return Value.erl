% --- dialyzer/test1.erl -----------------------
-module(test1).

-export([f1/0]).

f1() ->
  X = erlang:time(),
  seconds(X).

seconds({_Year, _Month, _Day, Hour, Min, Sec}) ->
  (Hour * 60 + Min)*60 + Sec.

% then

> dialyzer test1.erl
%% Checking whether the PLT /Users/joe/.dialyzer_plt is up-to-date... yes
%% Proceeding with analysis...
%% test1.erl:4: Function f1/0 has no local return
%% test1.erl:6: The call test1:seconds(X::
%%  {non_neg_integer(),non_neg_integer(),non_neg_integer()})
%%  will never return since it differs in the 1st argument
%%  from the success typing arguments: ({_,_,_,number(),number(),number()})
%% test1.erl:8: Function seconds/1 has no local return
%% test1.erl:8: The pattern {_Year, _Month, _Day, Hour, Min, Sec} can never
%%  match the type {non_neg_integer(),non_neg_integer(),non_neg_integer()}
%% done in 0m0.41s
