%% # ############# #
%% # Simple Macros #
%% # ############# #

% > Define Constant
-define(TIMEOUT, 1000).
%% then:
receive
  after ?TIMEOUT -> ok
end.


%% # #################### #
%% # Parameterized Macros #
%% # #################### #

% > The general form for parameterized macros is:

-define(Name(Var1,Var2,...,VarN), Replacement).

% sample:
-define(Multiple(X,Y),X rem Y == 0).

tstFun(Z,W) when ?Multiple(Z,W) -> true;
tstFun(Z,W) -> false.


%% # #################### #
%% # Debugging and Macros #
%% # #################### #

-define(VALUE(Call),io:format("~p = ~p~n",[??Call,Call])).
test1() -> ?VALUE(length([1,2,3])).

macros1: test1(). % "length ( [ 1 , 2 , 3 ] )" = 3


%% # ################## #
%% # Conditional macros #
%% # ################## #

-undef(Flag).
  % This will unset the Flag .
-ifdef(Flag).
  % If Flag is set, the statements that follow are executed.
-ifndef(Flag).
  % If Flag is not set, the statements that follow are executed.
-else.
  % This provides an alternative catch-all case: if this case is reached,
  % the statements that follow are executed.
-endif.
  % This terminates the conditional construct.

%% > Here is an example of their use:
-ifdef(debug).
  -define(DBG(Str, Args), io:format(Str, Args)).
-else.
  -define(DBG(Str, Args), ok).
-endif.

% To turn on system debugging, you need to set the debug flag. You can do this in the
% shell using the following command:
c(Module,[{d,debug}]).
% or
c(m1, {d, debug_flag}).



%% # ######################## #
%% # set of predefined macros #
%% # ######################## #

?MODULE % This expands to the name of the module in which it is used.
?MODULE_STRING % string consisting of the name of the module in which it is used.
?FILE % name of the file in which it is used.
?LINE % line number of the position at which it is used.
?MACHINE % name of the VM that is being used; currently, the only possible value for this is BEAM .

