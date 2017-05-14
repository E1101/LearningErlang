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
lists:map(Square,List). % [1,4,16,64,256,1024]
%% There’s another way to accomplish the same thing, with what Erlang calls a list comprehension.
[Square(Value) || Value <- List]. % [1,4,16,64,256,1024]
%% Create a list consisting of squares of a Value , where the Value comes from List.

