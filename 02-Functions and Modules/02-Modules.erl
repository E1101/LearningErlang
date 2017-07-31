%% Modules are a more formal place to put programs, and they give
%% you the ability to store, encapsulate, share, and manage
%% your code more effectively.

%% Modules are files with the extension .erl
%% and must be compiled before they can be run.


%% # ################# #
%% # Module Attributes #
%% # ################# #

% must be the first attribute in the file.
% ! Conventionally the code for modname should
%   be stored in a file called modname.erl .
-module(modname).
% Once a function has been imported from a module, then calling the
% function can be achieved without specifying the module name.
-import(Mod, [Name1/Arity1, Name2/Arity2,...]).
% Only exported functions can be called from outside a module.
-export([Name1/Arity1, Name2/Arity2, ...]).
% Options is a single compiler option or a list of compiler options
% ! The compiler option -compile(export_all). is often used
% while debugging programs.
-compile(Options).
% Version is any literal term. The value of Version
% has no particular syntax or meaning.
-vsn(Version).

% User-Defined Attributes; Can be extracted later!!
-SomeTag(Value).


-module(attrs).
-vsn(1234).
-author({joe,armstrong}).
-purpose("example of attributes").
-export([fac/1]).

mymodule:module_info().
%%[{exports,[{fac,1},{module_info,0},{module_info,1}]},
%%{imports,[]},
%%{attributes,[{vsn,[1234]},
%%{author,[{joe,armstrong}]},
%%{purpose,"example of attributes"}]}, <----- user-defined
%%{compile,[{options,[]},
%%{version,"4.8"}, <--- is the version of the compiler and should not be confused with the vsn tag
%%{time,{2013,5,3,7,36,55}},
%%{source,"/Users/joe/jaerlang2/code/attrs.erl"}]}]


% -- geometry.erl ---------------------------
-module(geometry).
-export([area/1]).

% The clauses are separated by a
% semicolon, and the final clause is terminated by dot whitespace.
area({rectangle, Width, Height})
  % function has no explicit return statement; the return value of the function is
  % simply the value of the last expression in the body of the clause.
  -> Width * Height;
area({square, Side})
  -> Side * Side.


%% # ############### #
%% # Compile Modules #
%% # ############### #

% -- hello.erl --------------------------
-module(hello).
-export([start/0]). % export start function with arity 0 that means no argument

start() ->
  io:format("Hello world~n").

$ erl
1> c(hello).
2> hello:start(). % Hello world

% > Compiling Outside the Erlang Shell

$ erlc hello.erl
$ erl -noshell -s hello start -s init stop % the expression init:stop() , which terminates the Erlang session.
% Hello world


%% # ############## #
%% # Define Modules #
%% # ############## #

% -- shop1.erl -------------------------------------
-module(shop1).
-export([total/1]).

total([{What, N}|T]) -> shop:cost(What) * N + total(T);
total([]) -> 0.

% then:

Buy = [{oranges,4}, {newspaper,1}, {apples,10}, {pears,6}, {milk,3}].
shop1:total(Buy).


% > Module for calculating and converting fall velocities [drop.el]

% ! module with name "drop" should declared in file names "drop.el"
-module(drop).
% which functions it should make visible to other code that uses this module.
% arity /1 mean fall_velocity with one(1) argument.
% Erlang considers functions with the same name but different arity to be different functions.
-export([fall_velocity/1, mps_to_mph/1, mps_to_kph/1]).
% help us keep track of which module version we have loaded in the runtime system at any point
-vsn(1.0).

% ! function names start with lowercase, not uppercase.
% ! fun and end don’t appear.
% FallVelocity = fun(Distance) -> math:sqrt(2 * 9.8 * Distance) end.
fall_velocity(Distance) -> math:sqrt(2 * 9.8 * Distance).

mps_to_mph(Mps) -> 2.23693629 * Mps.

mps_to_kph(Mps) -> 3.6 * Mps.


% > The shell will let you compile modules and then use them immediately.
% ! You need to be in the same directory as the file
ls().    % drop.erl
c(drop). % dont need to give .erl extension
ls().    % drop.erl drop.beam


% > Call modules functions
drop:fall_velocity(20).
drop:mps_to_mph(drop:fall_velocity(20)).


%% # ######################################### #
%% # Modules that call code from other modules #
%% # ######################################### #

-module(combined).
-export([height_to_mph/1]).

height_to_mph(Meters) -> convert:mps_to_mph(drop:fall_velocity(Meters)).


% > Using -import directive to reduce complexity
% ! not to use it unless you just can’t resist. It can make it harder to figure
% !-where bugs are coming from, which may cost you more time than the extra typing.
-module(combined).
-export([height_to_mph/1]).
-import(drop, [fall_velocity/1]).
-import(convert, [mps_to_mph/1]).

height_to_mph(Meters) -> mps_to_mph(fall_velocity(Meters)).



%% # ########### #
%% # Module Info #
%% # ########### #


-module(db).
-export([new/0,write/3,read/2, delete/2,destroy/1]).
-vsn(1.0).

new() -> dict:new().
write(Key, Data, Db) -> dict:store(Key, Data, Db).
read(Key, Db) ->
  case dict:fetch(Key, Db) of
    error
      -> {error, instance};
    {ok, Data} -> {ok, Data}
  end.
delete(Key, Db) -> dict:erase(Key, Db).
destroy(_Db) -> ok.

% then:

db:module_info().
%% [{exports,[{new,0},
%% {write,3},
%% {read,2},
%% {destroy,1},
%% {delete,2},
%% {module_info,0},
%% {module_info,1}]},
%% {imports,[]},
%% {attributes,[{vsn,[1.0]}]},
%% {compile,[{options,[{outdir,"/Users/Francesco/"}]},
%% {version,"4.5.2"},
%% {time,{2008,8,11,3,9,42}},
%% {source,"/Users/Francesco/db.erl"}]}]

db:module_info(attributes).
%% [{vsn,[1.1]}]


%% # ###################### #
%% # locate the object code #
%% # ###################### #

code:which(?MODULE).

code:which(file).
% "/usr/local/lib/erlang/lib/kernel-2.16.1/ebin/file.beam"

%% So The source can find here :
%% /usr/local/lib/erlang/lib/kernel-2.16.1/src/file.erl

