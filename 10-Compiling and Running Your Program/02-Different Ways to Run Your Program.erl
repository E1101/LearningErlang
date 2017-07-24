%
% > Quick Scripting

$ erl -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]).' \
-noshell -s init stop


% > Compile and Run from the Command Prompt

$ erlc hello.erl
$ erl -noshell -s hello start -s init stop

% -noshell Starts Erlang without an interactive shell.
% -s hello start Runs the function hello:start().
% -s init stop Stops the system by evaluating the function init:stop() after the previous command has finished.


% --- hello.sh --------------------------------------------
#!/bin/sh
erl -noshell -pa /home/joe/2012/book/JAERLANG/Book/code \
-s hello start -s init stop


% > Run As an Escript

% --- hello ------------------------------------------------
#!/usr/bin/env escript

% The file must contain a function main(Args).
% Args will contain a list of the command-line arguments represented as atoms.
main(Args) ->
  io:format("Hello world~n").


