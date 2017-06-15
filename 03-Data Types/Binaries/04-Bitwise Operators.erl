% The bit-level operators in Erlang can be applied to integers, returning integers as results.

%% Operator Description
band % Bitwise and
bor  % Bitwise or
bxor % Bitwise exclusive or
bnot % Bitwise negation
bsl  % Bit shift left; the second argument gives the size of the shift
bsr  % Bit shift right; the second argument gives the size of the shift
.

9 band 17. % 1
9 bor 17.  % 25
9 bxor 17. % 24
bnot (bnot 9). % 9
6 bsr 1. % 3
6 bsl 4. % 96

