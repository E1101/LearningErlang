%% What makes a true functional programming language is the fact that functions
%% can be handled just like any other sort of data.

%% Functional data types in Erlang are called funs. They can be passed as arguments to other
%% functions and they can be stored in data structures like tuples and records or sent as messages to
%% other processes.


%% # ################# #
%% # Block Expressions #
%% # ################# #

% are used when the Erlang syntax requires a single
% expression, but we want to have a sequence of expressions at this point in
% the code.

begin
  Expr1,
  ...,
  ExprN
end

