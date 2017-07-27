%% • Method 1: Store the same cookie in the file $HOME/.erlang.cookie . This file
%%   contains a random string and is automatically created the first time Erlang
%%   is run on your machine.
%%   This file can be copied to all machines that we want to participate in a
%%   distributed Erlang session.
$ cd
$ cat > .erlang.cookie
AFRTY12ESS3412735ASDF12378
$ chmod 400 .erlang.cookie

%% • Method 2: When Erlang is started, we can use the command-line argument
%%   -setcookie C to set the magic cookie to C . Here’s an example:
$ erl -setcookie AFRTY12ESS3412735ASDF12378 ...

%% • Method 3: The BIF erlang:set_cookie(node(), C) sets the cookie of the local node
%%   to the atom C .

