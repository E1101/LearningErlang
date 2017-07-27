%
% > We can explicitly generate an error by calling one of the following BIFs:


% to terminate the current process.
% If this exception is not caught, the signal {'EXIT',Pid,Why} will be broadcast to
% all processes that are linked to the current process.
exit(Why).

% to throw an exception that a caller might want to catch.
throw(Why).

% for denoting “crashing errors.”
% This is on par with internally generated errors.
error(Why).


% > Trapping an Exception with try...catch

try Exprs of
  Pattern1 [when Guard1] ->
    ExpressionBody1;
  Pattern2 [when Guard2] ->
    ExpressionBody2
catch
  %% ExceptionType (one of throw , exit , or error )
  %% ! defaults to throw.
  [ExceptionType:]ExceptionPattern1 % ExceptionPattern1 = why
    [when ExceptionGuardSeq1] ->
      ExceptionBody1;
  [ExceptionType:]ExceptionPatternN
    [when ExceptionGuardSeqN] ->
      ExceptionBodyN
after
  AfterExpressions
end


% > try...catch Has a Value

f(...) ->
...
X = try ... end,
Y = g(X),
...
% when we need no value from try..catch
f(...) ->
...
try ... end,
...
...


try F
catch
  ...
end


try Expr
catch
  _:_ -> ... % Code to handle all exceptions
  _ -> ... % Code to handle all exceptions
end


X=2.
try (X=3) of % badmatch error
  Val -> {normal, Val}
catch
    _:_ -> 43 % all error patterns in all classes
end. % 43

try (X=3) of
  Val -> {normal, Val}
catch
  error:Error -> {error,Error}
end. % {error,{badmatch,3}}


try (throw(non_normal_return)) of
  Val -> {normal, Val}
catch
  throw:Error -> {throw, Error}
end. % {throw,non_normal_return}


% --- try_test.erl  --------------------------------
generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

demo1() ->
  [catcher(I) || I <- [1,2,3,4,5]].
catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.
% --------------------------------------------------
% > then:
try_test:demo1().
%% [{1,normal,a},
%% {2,caught,thrown,a},
%% {3,caught,exited,a},
%% {4,normal,{'EXIT',a}},
%% {5,caught,error,a}]


% > Trapping an Exception with catch

list_to_integer("one"). % ** exception error: bad argument

catch list_to_integer("one").
%%{'EXIT',{badarg,[{erlang,list_to_integer,["one"]},
%%{erl_eval,do_apply,5},
%%{erl_eval,expr,5},
%%{shell,exprs,6},
%%{shell,eval_exprs,6},
%%{shell,eval_loop,3}]}}

X = catch 1/0. % * 1: syntax error before: 'catch'
X = (catch 1/0).
X.
%%{'EXIT',{badarith,[{erlang,'/',[1,0]},
%%{erl_eval,do_apply,5},
%%{erl_eval,expr,5},
%%{erl_eval,expr,5},
%%{shell,exprs,6},
%%{shell,eval_exprs,6},
%%{shell,eval_loop,3}]}}

