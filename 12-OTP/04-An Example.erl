-module(usr_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(FileName) ->
  UsrChild = {usr,{usr, start_link, []},
    permanent, 2000, worker, [usr, usr_db]},
  {ok,{{one_for_all,1,1}, [UsrChild]}}.


%% then:

usr_sup:start_link(). % {ok,<0.149.0>}
whereis(usr). % <0.150.0>
exit(whereis(usr), kill). % true

whereis(usr). % <0.156.0>
exit(whereis(usr), kill). % true
exit(whereis(usr), kill).
% ** exception exit: shutdown


