
try Exprs of
  Pattern1 [when Guard1] ->
    ExpressionBody1;
  Pattern2 [when Guard2] ->
    ExpressionBody2
catch
  [Class1:]ExceptionPattern1
    [when ExceptionGuardSeq1] ->
      ExceptionBody1;
  [ClassN:]ExceptionPatternN
    [when ExceptionGuardSeqN] ->
      ExceptionBodyN
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


% > Using try and catch to handle a possible error
-module(drop).
-export([fall_velocity/2]).

fall_velocity(Planemo, Distance) ->
  Gravity = case Planemo of
      earth -> 9.8;
      moon -> 1.6;
      mars -> 3.71
  end,

  try math:sqrt(2 * Gravity * Distance) of
    Result -> Result
  catch
    error:Error -> {error, Error}
  end.
  %% OR leave out the of clause entirely
  try math:sqrt(2 * Gravity * Distance)
  catch
    error:Error -> {error, Error}
  end.
% then:
drop:fall_velocity(earth,-20). % {error,badarith}

%% You can have multiple statements in the try
fall_velocity(Planemo, Distance) ->
  try
    Gravity = case Planemo of
        earth -> 9.8;
        moon -> 1.6;
        mars -> 3.71
    end,
    math:sqrt(2 * Gravity * Distance)
  of
    Result -> Result
  catch
    %% If your patterns don’t match the error in
    %% the catch clause, it gets reported as a runtime error
    error:Error -> {error, Error}
  end.
% then:
drop:fall_velocity(jupiter,20). % {error,{case_clause,jupiter}}


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

