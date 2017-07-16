%% Function specifications say what the types of the arguments to a function are
%% and what the type of the return value of the function is.

-spec functionName(T1, T2, ..., Tn) -> Tret when
  Ti :: Typei,
  Tj :: Typej,
...

-spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why} when
  FileName :: string(),
  Modes :: [Mode],
  Mode :: read | write | ...
  Handle :: file_handle(),
  Why :: error_term().

% or without when

-spec file:open(string(), [read|write|...] -> {ok, Handle} | {error, Why}


% > Type variables can be used in arguments, as in the following examples:

-spec lists:map(fun((A) -> B), [A]) -> [B].
-spec lists:filter(fun((X) -> bool()), [X]) -> [X].
% means that map takes a function from type A to B and list of objects of
% type A and returns a list of type B objects, and so on.

