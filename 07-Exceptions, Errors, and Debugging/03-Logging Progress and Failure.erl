%% By default, when Erlang starts up, it
%% sets up the error_logger module to report to the shell.

%% ! If you just want to write your errors
%% - to disk, you should explore the error_logger:logfile/1 function.

error_logger:info_msg("The value is ~p. ~n",[360]).

error_logger:warning_msg("Connection lost; will retry.").

error_logger:error_msg("Unable to read database.~n").
