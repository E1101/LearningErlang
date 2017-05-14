% Generally you will process a list in order,
% from the first item (the head) to the last item

% elements can be of any type, including numbers, atoms, tuples, strings, ...
[1,2,4,8,16,32]


% > You can pattern match with lists just as you can with other Erlang data structures:
[1,X,4,Y] = [1,2,4,8].
% X=2
% Y=8

Insert=[2,4,8].             % [2,4,8]
Full = [1, Insert, 16, 32]. % [1,[2,4,8],16,32]

Neat = lists:flatten(Full). % [1,2,4,8,16,32]

A = [1,2,4].
B = [8,16,32].
Combined1 = lists:append(A,B). % [1,2,4,8,16,32]
Combined2 = A ++ B.            % [1,2,4,8,16,32]
% ! The ++ operator is right associative, which can change the order of the
% - resulting list when you append multiple lists.

C = [64,128,256].
Combined4 = lists:append([A,B,C]). % [1,2,4,8,16,32,64,128,256]


% > generate a list of sequential
L = lists:seq(-2,8). % [-2,-1,0,1,2,3,4,5,6,7,8]
lists:seq($A,$z).    % "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz"

