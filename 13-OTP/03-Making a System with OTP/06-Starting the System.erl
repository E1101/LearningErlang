% ----- sellaprime.app -------------------------------------------
%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sellaprime,
  [{description, "The Prime Number Shop"},
  {vsn, "1.0"},
  {modules, [sellaprime_app, sellaprime_supervisor, area_server,
  prime_server, lib_lin, lib_primes, my_alarm_handler]},
  {registered,[area_server, prime_server, sellaprime_super]},
  {applications, [kernel,stdlib]},
  {mod, {sellaprime_app,[]}},
  {start_phases, []}
]}.


% Then we have to write a callback module with the same name as the mod file in the previous file.

% ----- sellaprime_app.erl -----------------------------------------

-module(sellaprime_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  sellaprime_supervisor:start_link(StartArgs).

stop(_State) -> ok.


% Then:

$ erl -boot start_sasl -config elog3

application:loaded_applications().
%%[{kernel,"ERTS CXC 138 10","2.16.1"},
%%{sasl,"SASL CXC 138 11","2.3.1"},
%%{stdlib,"ERTS CXC 138 10","1.19.1"}]

application:load(sellaprime).
application:loaded_applications().
application:start(sellaprime).
%%*** my_alarm_handler init:{xyz,{alarm_handler,[]}}
%%area_server starting
%%prime_server starting
application:stop(sellaprime).
application:unload(sellaprime).
