%% # ##################### #
%% # Reading Lines Of Text #
%% # ##################### #

% > Collecting user responses a line at a time

-module(ask).
-export([line/0]).

line() ->
  Planemo = get_planemo(),
  Distance = get_distance(),
  drop:fall_velocity({Planemo, Distance}).

get_planemo() ->
  io:format("Where are you?~n"),
  io:format(" 1. Earth ~n"),
  io:format(" 2. Earth's Moon~n"),
  io:format(" 3. Mars~n"),
  Answer = io:get_line("Which? > "), % returns the entire value the user entered, including the newline.
  Value = hd(Answer), % pulls the first item from a string or list
  char_to_planemo(Value).

get_distance() ->
  Input = io:get_line("How far? (meters) > "),
  Value = string:strip(Input, right, $\n),
  {Distance, _} = string:to_integer(Value),
  Distance.

char_to_planemo(Char) ->
  case
    [Char] == "1" -> earth; % evaluate the character as text
    Char == $2 -> moon;     % value for the character two
    Char == 51 -> mars      % character values
end.
% !! or:
% skip the case statement and just use pattern matching
char_to_planemo($1) -> earth;
char_to_planemo($2) -> moon;
char_to_planemo($3) -> mars.

