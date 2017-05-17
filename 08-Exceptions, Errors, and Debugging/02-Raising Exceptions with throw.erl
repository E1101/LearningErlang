%% It often takes a tuple as an argument, letting you provide more detail about
%% the exception, but you can use whatever you think appropriate.

throw(my_exception). % ** exception throw: my_exception

% > pattern match for thrown exceptions in a catch clause
try some:function(argument)
  catch
    error:Error -> {found, Error};
    throw:Exception -> {caught, Exception}
end;

