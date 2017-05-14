%%
%% Higher-order functions, functions that accept other functions as arguments

% ! Erlang not only lets you put functions into variables, it lets you pass functions as arguments.
Fall_velocity = fun(Distance) -> math:sqrt(2 * 9.8 * Distance) end.


% > An extremely simple higher-order function
-module(hof).
-export([tripler/2]).

tripler(Value, Function) -> 3 * Function(Value).
%% then:
MyFunction=fun(Value)->20*Value end.
hof:tripler(6,MyFunction). % 360
%% skip assigning the function to a variable if you want
hof:tripler(6,fun(Value)->20*Value end).


% > You may also want to pass a function from a module, even a built-in module, to your
% - (or any) higher-order function.
hof:tripler(math:pi(), fun math:cos/1). % -3.0


