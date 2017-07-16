%% # ######################## #
%% # Exported and Local Types #
%% # ######################## #

-module(a).

-type rich_text() :: [{font(), char()}].
-type font() :: integer().

-export_type([rich_text/0, font/0]).
...

% then on module b

-module(b).
...
-spec rich_text_length(a:rich_text()) -> integer().
...

