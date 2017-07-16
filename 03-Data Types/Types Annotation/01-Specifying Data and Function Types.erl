%
% ---- walks.erl ------------------------
-module(walks).
-export([plan_route/2]).

-spec plan_route(point(), point()) -> route().
% or more expressive power
% They are also used to link the names in the documentation
% to the variables in the type annotations.
-spec plan_route(From:: point(), To:: point()) -> route().

-type direction() :: north | south | east | west.
-type point() :: {integer(), integer()}.
-type route() :: [{go,direction(),integer()}].

...

% Plan Route Can Be Used as Something like this:
walks:plan_route({1,10}, {25, 57}).
% [{go, east, 24},
%  {go, north, 47},
%  ...
% ]


-type angle() :: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.
-type position() :: {latitude | longitude, angle()}.

-spec plan_route1(From::position(), To::position()) -> ...


any().  % means any Erlang term.
none(). % is used to denote the type of a function that never returns.

% > New types can be defined with the following syntax:
-type NewTypeName(TVar1, TVar2, ... TVarN) :: Type.

-type onOff()        :: on | off.
-type person()       :: {person, name(), age()}.
-type people()       :: [person()].
-type name()         :: {firstname, string()}. % say {firstname, "dave"} is of type name().
-type age()          :: integer().
-type dict(Key,Val)  :: [{Key,Val}]. % defines a dictionary type to be a list of {Key, Val} tuples.


%% # ################ #
%% # Predefined Types #
%% # ################ #

-type term() :: any().
-type boolean() :: true | false.
-type byte() :: 0..255.
-type char() :: 0..16#10ffff.
-type number() :: integer() | float().
-type list() :: [any()].
% specify the types of lists whose ultimate tail is non-nil
-type maybe_improper_list() :: maybe_improper_list(any(), any()).
-type maybe_improper_list(T) :: maybe_improper_list(T, any()).
-type string() :: [char()].
-type nonempty_string() :: [char(),...]. % means a non-empty list of type x
-type iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []).
-type module() :: atom().
-type mfa() :: {atom(), atom(), atom()}.
-type node() :: atom().
-type timeout() :: infinity | non_neg_integer().
-type no_return() :: none().

