%

% > Reporting on a List
Print = fun(Value) -> io:format("~p~n",[Value]) end.
List = [1,2,4,8,16,32].
lists:foreach(Print,List).
% 1
% 2
% 4
% ..
% ok
% ! When it reached the end of the list, lists:fore
% - ach/2 returned the value ok


% > Manipulating List Values
Square = fun(Value)->Value*Value end.
lists:map(Square,List).           % [1,4,16,64,256,1024]
%% There’s another way to accomplish the same thing, with what Erlang calls a list comprehension.
[Square(Value) || Value <- List]. % [1,4,16,64,256,1024]
%% Create a list consisting of squares of a Value , where the Value comes from List.


% > Filtering List Values
Four_bits = fun(Value)-> (Value<16) and (Value>=0) end.
lists:filter(Four_bits,List).                 % [1,2,4,8]
%% same as
[Value || Value <- List, Value<16, Value>=0]. % [1,2,4,8]


% > Testing Lists
IsInt = fun(Value) -> is_integer(Value) end.
lists:all(IsInt, List). % true

Compare = fun(Value) -> Value > 10 end.
lists:any(Compare, List). % true
lists:all(Compare, List). % false


% > Splitting Lists
lists:partition(Compare,List). % {[16,32],[1,2,4,8]}

Test=fun(Value) -> Value < 4 end.
lists:dropwhile(Test, [1,2,4,8,4,2,1]). % [4,8,4,2,1]
lists:takewhile(Test, [1,2,4,8,4,2,1]). % [1,2]
