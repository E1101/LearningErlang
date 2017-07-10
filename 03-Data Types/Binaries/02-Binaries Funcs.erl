
list_to_binary(L) -> B
  Bin1 = <<1,2,3>>. % Without this space, the second symbol seen by the Erlang tokenizer would be the atom '=<'
  Bin2 = <<4,5>>.
  Bin3 = <<6>>.
  list_to_binary([Bin1,1,[2,3,Bin2],4|Bin3]).
  % <<1,2,3,1,2,3,4,5,4,6>>

split_binary(Bin, Pos) -> {Bin1, Bin2}
  split_binary(<<1,2,3,4,5,6,7,8,9,10>>, 3).
  % {<<1,2,3>>,<<4,5,6,7,8,9,10>>}

term_to_binary(Term) -> Bin
  % Terms can be stored in files, sent in messages over a network, and
  % so on, and the original term from which they were made can be recon-
  % structed later.

binary_to_term(Bin) -> Term
  % This is the inverse of term_to_binary .
  B = term_to_binary({binaries,"are", useful}).
  binary_to_term(B).
  % {binaries,"are",useful}

byte_size(Bin) -> Size
  byte_size(<<1,2,3,4,5>>). % 5


% >> Once we have converted a term to a binary, we can send
%    the binary in a message over a socket or store it in a file.


Bin1 = term_to_binary({test,12,true,[1,2,3]}).
% <<131,104,4,100,0,4,116,101,115,116,97,12,100,0,4,116,114,117,101,107,0,3,1,2,3>>
Term1 = binary_to_term(Bin1). % {test,12,true,[1,2,3]}

Bin2 = term_to_binary({cat,dog}).
% <<131,104,2,100,0,3,99,97,116,100,0,3,100,111,103>>
Bin3 = list_to_binary([Bin1, Bin2]).
% <<131,104,4,100,0,4,116,101,115,116,97,12,100,0,4,116,114,
% 117,101,107,0,3,1,2,3,131,104,2,100,...>>
Term2 = binary_to_term(Bin3). % {test,12,true,[1,2,3]}

{Bin4,Bin5} = split_binary(Bin3,25).
% {<<131,104,4,100,0,4,116,101,115,116,97,12,100,0,4,116,114,117,101,107,0,3,1,2,3>>,
% <<131,104,2,100,0,3,99,97,116,100,0,3,100,111,103>>}
Term4 = binary_to_term(Bin5). % {cat,dog}

is_binary(Term4). % false
is_binary(Bin4).  % true

