% There are two kinds of applications.
% - normal applications: will start the supervision tree and all of the relevant static workers.
% - Library applications: they are started as part of a supervision tree belonging to another application.

% > To find out which applications are running in your Erlang runtime system:
application:which_applications().
%%[{stdlib,"ERTS CXC 138 10","1.15.2"},
%%{kernel,"ERTS CXC 138 10","2.12.2"}]


%% # ################### #
%% # Directory Structure #
%% # ################### #

% > src
%   source code of all the Erlang modules in the application.
% > ebin
%   all of the compiled beam files and the application resource file
% > include
%   all the Erlang header files ( hrl ) intended for use outside the application.
% > priv
%   optional directory that contains necessary scripts, graphics, configuration files,
%   or other non-Erlang-related resources.

%% # ############################# #
%% # The Application Resource File #
%% # ############################# #

% contains information on your application resources and dependencies.

%% --- ebin/name.app

%% the application tag, application name, list of features
{application, inets,
[{description, "INETS CXC 138 49"},
%% should be the same as the suffix of the application directory
{vsn, "5.0.5"},
%% to ensure that all of them are present when building
%% the system and that there are no name clashes with any other applications. The second
%% is to be able to load them either at startup or when loading the application. For every
%% module, there should be a corresponding beam file.
{modules, [inets, inets_sup, inets_app, inets_service,
ftp, ftp_progress, ftp_response, ftp_sup,
http, httpc_handler,httpc_handler_sup, httpc_manager,
tftp, tftp_binary, tftp_engine,tftp_file, tftp_lib, tftp_sup
]},
{registered, [inets_sup, httpc_manager]},
%% to be started after other applications on which they depend.
{applications, [kernel, stdlib]},
%% callback module and the arguments passed to the start/2 callback function.
{mod, {inets_app, []}}]}.

% > env tag indicates a list of key-value tuples that can be accessed from within the application
application:get_env(Tag)
application:get_all_env().
% other applications
application:get_env(Name,Tag)
application:get_all_env(Name).



{application, usr,
[{description, "Mobile Services Database"},
{vsn, "1.0"},
{modules, [usr, usr_db, usr_sup, usr_app]},
{registered, [usr, usr_sup]},
{applications, [kernel, stdlib]},
{env, [{dets_name, "usrDb"}]},
{mod, {usr_app,[]}}]}.


%% # ################################## #
%% # Starting and Stopping Applications #
%% # ################################## #

application:start(ApplicationName).
% {ok, Pid} or {ok, Pid, Data}
application:stop(ApplicationName).

% > Start
%%  the start(StartType, Arguments) function in the application callback
%%  module is invoked.
%%  > StartType
%%    usually the atom normal, takeover and failover on distributed systems.

-module(usr_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  usr_sup:start_link().

stop(_State) ->
  ok.


%% # ####################### #
%% # The Application Monitor #
%% # ####################### #

  appmon:start().

