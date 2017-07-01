%%
%% Higher-order functions, functions that accept other functions as arguments

%% Functions that manipulate functions are called higher-order
%% functions, and the data type that represents a function in Erlang is called a
%% fun.


% ! Erlang not only lets you put functions into variables, it lets you pass functions as arguments.
Fall_velocity = fun(Distance) -> math:sqrt(2 * 9.8 * Distance) end.


% Funs can have several different clauses.
TempConvert = fun
  ({c,C}) -> {f, 32 + C*9/5};
  ({f,F}) -> {c, (F-32)*5/9}
end.

TempConvert({c,100}). % {f,212.0}
TempConvert({f,212}). % {c,100.0}


% > Functions That Have Funs As Their Arguments

Even = fun(X) -> (X rem 2) =:= 0 end. % =:= is a test for equality
lists:map(Even, [1,2,3,4,5,6,8]).
% [false,true,false,true,false,true,true]
lists:filter(Even, [1,2,3,4,5,6,8]).
% [2,4,6,8]


% > Functions That Return Funs

Fruit = [apple,pear,orange].

MakeTest = fun(L) -> (fun(X) -> lists:member(X, L) end) end.
IsFruit = MakeTest(Fruit).

IsFruit(pear). % true
IsFruit(dog).  % false

lists:filter(IsFruit, [dog,orange,cat,apple,bear]).
% [orange,apple]


% > Defining Your Own Control Abstractions
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

for(1,10,fun(I) -> I end).
% [1,2,3,4,5,6,7,8,9,10]


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


