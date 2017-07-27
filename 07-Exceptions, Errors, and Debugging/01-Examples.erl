%
% > Using try and catch to handle a possible error

% -------
sqrt(X) when X < 0 ->
  error({squareRootNegativeArgument, X});
sqrt(X) ->
  math:sqrt(X).
% -------



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

