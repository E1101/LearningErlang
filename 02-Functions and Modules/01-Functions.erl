%% # ################# #
%% # Create a function #
%% # ################# #

% > binding the function to the variable
FallVelocity = fun(Distance) -> math:sqrt(2 * 9.8 * Distance) end.

FallVelocity(20). % 19.79898987322333


Bump = fun(Int) -> Int + 1 end.
% #Fun<erl_eval.6.13229925>

Bump(10). % 11

(fun(Int) -> Int + 1 end)(9). % 10


%% # ################## #
%% # From Module to Fun #
%% # ################## #

% Within a module M , a local function F with
% arity n can be denoted by fun F/n .
F_v = fun drop:fall_velocity/1.
F_v(20).


%% # ###################### #
%% # Functions As Arguments #
%% # ###################### #

map(F,[]) -> [];
map(F,[X|Xs]) -> [F(X) | map(F,Xs)].

doubleAll(Xs) ->
  map( fun(X) -> X*2 end , Xs).


%% # #################### #
%% # Functions As Results #
%% # #################### #

times(X) ->
  fun (Y) -> X*Y end.

Times = hof1:times(3).
(hof1:times(3))(2). % 6
Times(2). % 6


sendTo(Pid) ->
  fun (X) ->
    Pid ! X
  end.


%% # ################ #
%% # Pattern Matching #
%% # ################ #

func1([{tag1, A, B}|T]) ->
  ...
  ... f(..., {tag1, A, B}, ...)
...

% More efficient way is:

func1([{tag1, A, B}=Z|T]) ->
  ...
  ... f(... Z, ...)
...


func1([{tag, {one, A}=Z1, B}=Z2|T]) ->
  ..,.
  ... f(..., Z2, ...),
  ... g(..., Z1, ...),
...


%% # ##################################### #
%% # Pattern Matching Records in Functions #
%% # ##################################### #

clear_status(#todo{status=S, who=W} = R) ->
  %% Inside this function S and W are bound to the field
  %% values in the record
  %%
  %% R is the *entire* record
  R#todo{status=finished}
  %% ...
.

do_something(X) when is_record(X, todo) ->
  %% ...
.


% > The BIF apply(Mod, Func, [Arg1, Arg2, ..., ArgN])

apply(erlang, atom_to_list, [hello]). % hello

