%% # ####################### #
%% # Mixing Lists and Tuples #
%% # ####################### #

List1=[1,2,4,8,16].
List2=[a,b,c,d,e].

TupleList=lists:zip(List1,List2).     % [{1,a},{2,b},{4,c},{8,d},{16,e}]
SeparateLists=lists:unzip(TupleList). % {[1,2,4,8,16],[a,b,c,d,e]}


Initial=[{1,tiger}, {3,bear}, {5,lion}].
Second=lists:keystore(7,1,Initial,{7,panther}). % [{1,tiger},{3,bear},{5,lion},{7,panther}]
Third=lists:keystore(7,1,Second,{7,leopard}).   % [{1,tiger},{3,bear},{5,lion},{7,leopard}]


% > replace a value only if it is present
Fourth=lists:keyreplace(6,1,Third,{6,chipmunk}).% [{1,tiger},{3,bear},{5,lion},{7,leopard}]
% ! There was no item in the previous list with a key value of 6 , so lists:keyreplace/4
% - just returned a copy of the original list.


% > find
Animal5=lists:keyfind(5,1,Third). % {5,lion}
Animal6=lists:keyfind(6,1,Third). % false


% > pascal triangle
-module('03-Get Deeper With List Funcs').
-export([triangle/1]).

triangle(Rows) -> triangle([[0,1,0]],1,Rows).
triangle(List, Count, Rows) when Count >= Rows -> lists:reverse(List);
triangle(List, Count, Rows) ->
  [Previous | _] = List,
  triangle([add_row(Previous) | List], Count+1, Rows).

add_row(Initial) -> add_row(Initial, 0, []).
add_row([], 0, Final) -> [0 | Final];
add_row([H | T], Last, New) -> add_row(T, H, [Last + H | New]).

% then:
pascal:triangle(6).
% [[0,1,0],
% [0,1,1,0],
% [0,1,2,1,0],
% [0,1,3,3,1,0],
% [0,1,4,6,4,1,0],
% [0,1,5,10,10,5,1,0]]
