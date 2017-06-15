ets:select(countries,
   [{{'$1','$2','$3'},[{'/=','$3',cook}],[['$2','$1']]}]).
% [[ireland,sean],[ireland,chris]]

MS = ets:fun2ms(fun({Name,Country,Job}) when Job /= cook
   -> [Country,Name] end).
% [{{'$1','$2','$3'},[{'/=','$3',cook}],[['$2','$1']]}]

ets:select(countries, MS).
% [[ireland,sean],[ireland,chris]]


