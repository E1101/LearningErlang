Bin = <<E1, E2, ...,En>>
% pattern-match them
<<E1, E2, ...,En>> = Bin

Bin1 = <<1,2,3>>.

binary_to_list(Bin1).
% [1,2,3]

<<E,F>> = Bin1.
% ** exception error: no match of right hand side value <<1,2,3>>

<<E,F,G>> = Bin1.
% <<1,2,3>>

E. % 1

%% ease of list-style manipulation of binaries

[B|Bs] = binary_to_list(Bin1).
% [1,2,3]

Bin2 = list_to_binary(Bs).
% <<2,3>>


%% # ################## #
%% # Type qualification #
%% # ################## #

Expr:Size/Type

<<5:4, 5:4>>. % <<"U">>
%% integer 5 represented in four bits is equivalent to 0101
%% we put two of them together, 01010101. which is the
%% integer 85, denoting the ASCII value of U.

<<Int1:2, Int2:6>> = <<128>>.
Int1. % 2
Int2. % 0


A = 1.
Bin = <<A, 17, 42:16>>. % <<1,17,0,42>>

<<D:16,E,F/binary>> = Bin.
[D,E,F]. % [273,0,<<"*">>]


Frame = <<1,3,0,0,1,0,0,0>>.
<<Type, Size, Bin:Size/binary-unit:8, _/binary>> = Frame.

Type. % 1
Size. % 3
Bin.  % <<0,0,1>>

% > It is possible to match bitstrings of any length
<<X:7/bitstring,Y:1/bitstring>> = <<42:8>>.

X. % <<21:7>>
Y. % <<0:1>>


%% Consideration:

<<X:7/binary,Y:1/binary>>
% will never match, as each binary sequence in a pattern
% match must have a length that is a multiple of 8.

