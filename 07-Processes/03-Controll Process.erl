% > A process that sends a message back to the process that called it
-module(drop).
-export([drop/0]).

drop() ->
  receive
    {From, Planemo, Distance} ->
      From ! {Planemo, Distance, fall_velocity(Planemo, Distance)},
      drop()
  end.

fall_velocity(earth, Distance) when Distance >= 0 -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 -> math:sqrt(2 * 3.71 * Distance).
% then:
Pid1=spawn(drop,drop,[]).
Pid1 ! {self(), moon, 20}. % {<0.31.0>,moon,20}
flush(). % Shell got {moon,20,8.0}


% > Calling a process from a process, and reporting the results
-module(mph_drop).
-export([mph_drop/0]).

mph_drop() ->
  Drop=spawn(drop,drop,[]),
  convert(Drop).

convert(Drop) ->
  receive
    {Planemo, Distance} ->
      Drop ! {self(), Planemo, Distance},
      convert(Drop);
    {Planemo, Distance, Velocity} ->
      MphVelocity= 2.23693629 * Velocity,
      io:format("On ~p, a fall of ~p meters yields a velocity of ~p mph.~n",
        [Planemo, Distance, MphVelocity]),
      convert(Drop)
  end.
% then:
Pid1=spawn(mph_drop,mph_drop,[]).
Pid1 ! {earth,20}.
% On earth, a fall of 20 meters yields a velocity of 44.289078952755766 mph.


% > Trapping a failure, reporting an error, and exiting
-module(mph_drop).
-export([mph_drop/0]).

mph_drop() ->
  %% When an Erlang process fails, itsends an explanation to
  %% other processes that are linked to it in the form of a tuple.
  process_flag(trap_exit, true),
  Drop=spawn_link(drop,drop,[]),
  convert(Drop).

convert(Drop) ->
  receive
    {Planemo, Distance} ->
      Drop ! {self(), Planemo, Distance},
      convert(Drop);
    %% process is set to trap exits, errors reports arrive as messages, rather than just killing your process.
    {'EXIT', Pid, Reason} ->
      NewDrop=spawn_link(drop,drop,[]),
      io:format("FAILURE: ~p died because of ~p.~n",[Pid, Reason]),
      convert(NewDrop);
    {Planemo, Distance, Velocity} ->
      MphVelocity= 2.23693629 * Velocity,
      io:format("On ~p, a fall of ~p meters yields a velocity of ~p mph.~n",[Planemo, Distance, MphVelocity]),
      convert(Drop)
  end.

