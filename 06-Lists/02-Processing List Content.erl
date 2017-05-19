% > extract the head and the tail
[Head | Tail] = [1,2,4].
Head. % 1
Tail. % [2,4]
% ! value is [] when Tail is empty

% > need to go through a list backwards, you’ll need to use
% - the lists:reverse function and then walk through the reversed list.

lists::reverse([1,2,3]).


% > Calculating the product of values in a list
-module(overall).
-export([product/1]).

product([]) -> 0; % in case the list is empty, return zero
product(List) -> product(List,1).

product([], Product) -> Product; % when list empty, stop, report
product([Head|Tail], Product) -> product(Tail, Product * Head).

%% # ################################### #
%% # Creating Lists with Heads and Tails #
%% # ################################### #

X=[1|[2,3]].   % [1,2,3]
Y=[1,2 | [3]]. % [1,2,3]



% > Calculating a series of drop velocities
-module(listdrop).
-export([falls/1]).

falls(List) -> falls(List,[]).
falls([], Results) -> lists:reverse(Results); % finished result, because recursion call backwards
falls([Head|Tail], Results) -> falls(Tail, [drop:fall_velocity(Head) | Results]).
% then:
listdrop:falls([{earth,20},{moon,20},{mars,20}]).
% [19.79898987322333,8.0,12.181953866272849]


