%% When you start programming in Erlang, you’ll probably put all your modules
%% and files in the same directory and start Erlang from this directory. If you do
%% this, then the Erlang loader will have no trouble finding your code. However,
%% as your applications become more complex, you’ll want to split them into
%% manageable chunks and put the code into different directories. And when
%% you include code from other projects, this external code will have its own
%% directory structure.


%% # ######################## #
%% # manipulate the load path #
%% ########################## #

%% If the missing module is called myMissingModule , then the code
%% loader will search for a file called myMissingModule.beam in all the directories that
%% are in the current load path.

code:get_path().
%%[".",
%%"/usr/local/lib/erlang/lib/kernel-2.15/ebin",
%%"/usr/local/lib/erlang/lib/stdlib-1.18/ebin",
%%"/home/joe/installed/proper/ebin",
%%"/usr/local/lib/erlang/lib/xmerl-1.3/ebin",
%%"/usr/local/lib/erlang/lib/tools-2.6.6.6/ebin",
%%...]

-spec code:add_patha(Dir) => true | {error, bad_directory} % to the start of the load path.
-spec code:add_pathz(Dir) => true | {error, bad_directory} % to the end of the load path.


% Alternatively, you can start Erlang with a command like this:

$ erl -pa Dir1 -pa Dir2 ... -pz DirK1 -pz DirK2


%% # ###################################################### #
%% # Executing a Set of Commands When the System Is Started #
%% ######################################################## #

init:get_argument(home). % {ok,[["/home/joe"]]}

%% If there is a file called .erlang in the current directory when Erlang is started,
%% then it will take precedence over the .erlang in your home directory.

