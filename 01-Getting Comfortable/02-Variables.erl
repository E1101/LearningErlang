%% # ###################### #
%% # Working with variables #
%% # ###################### #

% > variable names begin with a capital letter or an underscore.
% > whereas underscores start “don’t care” variables.

N=1.
N. % 1

% > variables only assign once
N=2. % ** exception error: no match of right hand side value 2
% whereas if match previous:
N=2-1. % 1
N=15 div (3*5). % 1

