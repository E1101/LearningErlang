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


%% # ######################### #
%% # Lazy Evaluation and Lists #
%% # ######################### #

%% The Erlang evaluation mechanism means that when a list is passed to a function,
%% the list is evaluated fully before the function body is executed.

next(Seq) ->
  fun() -> [Seq|next(Seq+1)] end.


%% # ################### #
%% # List Comprehensions #
%% # ################### #

[X+1 || X <- [1,2,3]]. % [2,3,4]

[X || X <- [1,2,3], X rem 2 == 0]. % [2]

[X+1 || X <- [1,2,3], X rem 2 == 0]. %


Database = [ {francesco, harryPotter}, {simon, jamesBond},
{marcus, jamesBond}, {francesco, daVinciCode} ].

[Person || {Person,_} <- Database]. % [francesco,simon,marcus,francesco]
[Book || {Person,Book} <- Database, Person == francesco]. % [harryPotter,daVinciCode]
[Book || {francesco,Book} <- Database]. % [harryPotter,daVinciCode]
[Person || {Person,daVinciCode} <- Database]. % [francesco]
[Book || {Person,Book} <- Database, Person /= marcus]. % [harryPotter,jamesBond,daVinciCode]
[Person || {Person,Book} <- Database, Person /= marcus]. % [francesco,simon,francesco]

[{Book, [Person || {Person,B} <- Database, Book==B ]} || {_,Book} <- Database].
% [{harryPotter,[francesco]},
% {jamesBond,[simon,marcus]},
% {jamesBond,[simon,marcus]},
% {daVinciCode,[francesco]}]

[{Book,[ Person || {Person,Book} <- Database ]} || {_,Book} <- Database].
% [{harryPotter,[francesco,simon,marcus,francesco]},
% {jamesBond,[francesco,simon,marcus,francesco]},
% {jamesBond,[francesco,simon,marcus,francesco]},
% {daVinciCode,[francesco,simon,marcus,francesco]}]

[ {X,Y} || X <- lists:seq(1,3), Y <- lists:seq(X,3) ]. % [{1,1},{1,2},{1,3},{2,2},{2,3},{3,3}]
[ {X,Y} || X <- lists:seq(1,4), X rem 2 == 0, Y <- lists:seq(X,4) ]. % [{2,2},{2,3},{2,4},{4,4}]
[ {X,Y} || X <- lists:seq(1,4), X rem 2 == 0, Y <- lists:seq(X,4), X+Y>4 ]. % [{2,3},{2,4},{4,4}]



