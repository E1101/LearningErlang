% The formatting string contains characters that are printed as they
% - are with control sequences for formatting.

% > An ASCII code to be printed as a character.
~c

% > A float to be printed with six decimal places.
~f

% > A float to be printed in scientific notation, showing six digits in all.
~e

% > Writes any term in standard syntax.
~w

% > Writes data as ~w , but in “pretty printing” mode, breaking lines in appropriate
% - places, indenting sensibly, and outputting lists as strings where possible.
~p

% > Behave as ~w , ~p , but eliding structure at a depth of 3. These take an extra argument
% - in the data list indicating the maximum depth for printing terms.
~W , ~P

% > Shows an integer to base 10.
~B


List = [2,3,math:pi()].          % [2,3,3.141592653589793]
Sum = lists:sum(List).           % 8.141592653589793
io:format("hello, world!~n",[]). % hello, world!

io:format("the sum of ~w is ~w.~n", [[2,3,4],ioExs:sum([2,3,4])]).
% the sum of [2,3,4] is 9.

io:format("the sum of ~w is ~w.~n", [List,Sum]).
% the sum of [2,3,3.141592653589793] is 8.141592653589793.

io:format("the sum of ~W is ~w.~n", [List,3,Sum]).
% the sum of [2,3|...] is 8.141592653589793.

io:format("the sum of ~W is ~f.~n", [List,3,Sum]).
% the sum of [2,3|...] is 8.141593.

io:format("the sum of ~W is ~.2f.~n", [List,3,Sum]).
% the sum of [2,3|...] is 8.14.

