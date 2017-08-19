
% Here’s a simple subset of the API:

-spec error_logger:error_msg(String) -> ok.

-spec error_logger:error_msg(Format, Data) -> ok.
% The arguments are the same as for io:format(Format, Data) .

-spec error_logger:error_report(Report) -> ok.
% Send a standard error report to the error logger.
  -type Report = [{Tag, Data} | term() | string() ] .
  -type Tag = term() .
  -type Data = term() .

error_logger:error_report([{tag1,data1},a_term,{tag2,data}]).


%% # ############################ #
%% # Configuring the Error Logger #
%% # ############################ #

% When we start Erlang, we can give the system a boot argument.

$ erl -boot start_clean
%% This creates an environment suited for program development. Only a
%% simple form of error logging is provided.

%% The command erl with no boot argument is equivalent to
%% erl -boot start_clean .

$ erl -boot start_sasl
%% This creates an environment suitable for running a production system.


% ----- elog1.config --------------------------------------------
%% no tty
[{sasl, [
    {sasl_error_logger, false}
  ]}].

%% If we start the system with this configuration file, we’ll get only error reports
%% and not progress reports and so on. All these error reports are only in the shell.
$ erl -boot start_sasl -config elog1


% ----- elog2.config ---------------------------------------------
%% single text file - minimal tty
[{sasl, [
    %% All reports go to this file
    {sasl_error_logger, {file, "/Users/joe/error_logs/THELOG"}}
  ]}].

%% The next configuration file lists error reports in the shell, and all progress
%% reports are saved in a file.
%% the errors reported by error_logger:error_msg/1 are not saved in
%% the log. For this we have to configure a rotating log.
$ erl -boot start_sasl -config elog2


% ----- elog3.config ----------------------------------------------
%% rotating log and minimal tty
[{sasl, [
  {sasl_error_logger, false},
  %% define the parameters of the rotating log
  %% the log file directory
  {error_logger_mf_dir,"/Users/joe/error_logs"},
  %% # bytes per logfile
  {error_logger_mf_maxbytes,10485760}, % 10 MB
  %% maximum number of logfiles
  {error_logger_mf_maxfiles, 10}
]}].

%% gives us shell output plus a copy of everything
%% that was written to the shell in a rotating log file.
$ erl -boot start_sasl -config elog3


% ----- elog4.config ---------------------------------------------
%% rotating log and errors
[{sasl, [
  %% minimise shell error logging
  {sasl_error_logger, false},
  %% only report errors
  {errlog_type, error},
  %% define the parameters of the rotating log
  %% the log file directory
  {error_logger_mf_dir,"/Users/joe/error_logs"},
  %% # bytes per logfile
  {error_logger_mf_maxbytes,10485760}, % 10 MB
  %% maximum number of
  {error_logger_mf_maxfiles, 10}
]}].

%% In a production environment, we are really interested only in errors and not
%% progress or information reports, so we tell the error logger to report only
%% errors.
$ erl -boot start_sasl -config elog4

