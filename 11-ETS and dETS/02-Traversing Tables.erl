First = ets:first(indexTable). % {"appears",2}
Second = ets:next(indexTable,First).

Last = ets:last(indexTable).
ets:next(indexTable,Last). % '$end_of_table'


% ! In Bag tables the order is determined by hash table order


%% # ################################# #
%% # ETS Tables and Concurrent Updates #
%% # ################################# #

ets:safe_fixtable(TableRef, Flag)
%% If you know other processes will be executing destructive calls to the ETS table while
%% you are traversing it using ets:first/1 and ets:next/2 , use the function
%% ets:safe_fixtable/2 .
%% where Flag is set to true . The flag is released either by setting the flag back to false ,
%% or upon process termination. If several processes fix a table, it will remain fixed until
%% all processes have either released it or terminated.

