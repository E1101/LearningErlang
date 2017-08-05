cd("/home/joe/book/erlang/Book/code").

file:list_dir(".").
%%{ok,["id3_v1.erl~",
%%  "update_binary_file.beam",
%%  "benchmark_assoc.beam",
%%  "id3_v1.erl",
%%  "scavenge_urls.beam",
%%  "benchmark_mk_assoc.beam",
%%  "benchmark_mk_assoc.erl",
%%  "id3_v1.beam",
%%  "assoc_bench.bea"
%%  ..


% > Finding Information About a File

{ok, Info} = file:read_file_info(F) .

-record(file_info,
{size,    % Size of file in bytes.
  type,   % Atom: device, directory, regular,
          % or other.
  access, % Atom: read, write, read_write, or none.
  atime,  % The local time the file was last read:
          % {{Year, Mon, Day}, {Hour, Min, Sec}}.
  mtime,  % The local time the file was last written.
  ctime,  % The interpretation of this time field
          % is dependent on operating system.
          % On Unix it is the last time the file or
          % or the inode was changed. On Windows,
          % it is the creation time.
  mode,   % Integer: File permissions. On Windows,
          % the owner permissions will be duplicated
          % for group and user.
  links,  % Number of links to the file (1 if the
          % filesystem doesn't support links).
...
}).


-include_lib("kernel/include/file.hrl").

file_size_and_type(File) ->
  case file:read_file_info(File) of
    {ok, Facts} ->
      {Facts#file_info.type, Facts#file_info.size};
    _ ->
      error
  end.

ls(Dir) ->
  {ok, L} = file:list_dir(Dir),
  lists:map(fun(I) -> {I, file_size_and_type(I)} end, lists:sort(L)).


% ! If we just want to get the size of a file, it is more convenient to
% call filelib:file_size than to call file:read_file_info and unpack the
% elements of the #file_info record.


% > Copying and Deleting Files

%% copies the file Source to Destination .
file:copy(Source, Destination)
%% deletes File .
file:delete(File)

