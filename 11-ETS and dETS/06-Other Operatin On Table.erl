ets:tab2file(TableId | TableName , FileName)
% dumps a table into a file, returning ok or {error, Reason} .

ets:file2tab(FileName)
% reads a dumped table back in, returning {ok, Tab} or {error, Reason} .

ets:tab2list(TableId | TableName)
% returns a list containing all the elements of the table.

ets:i()
% list summary information about all the ETS tables visible from the current process.

ets:info(TableId | TableName)


%% # ################## #
%% # Visualizing Tables #
%% # ################## #

%% tables owned by both the current node and connected nodes are shown when
%% the visualizer is launched.

tv:start().

