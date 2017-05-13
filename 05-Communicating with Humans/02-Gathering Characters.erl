%% # #################### #
%% # Gathering Characters #
%% # #################### #

% > Presenting a menu and waiting for a single character response
-module('01-Gathering Terms').
-export([chars/0]).

chars() ->
  % you could combine all io:format/1 to a single call
  io:format("Which planemo are you on?~n"),
  io:format(" 1. Earth ~n"),
  io:format(" 2. Earth's Moon~n"),
  io:format(" 3. Mars~n"),
  io:get_chars("Which? > ",1).

%% ask:chars().
% Which planemo are you on?
% 1. Earth
% 2. Earth's Moon
% 3. Mars
% Which? > 3
% "3"
