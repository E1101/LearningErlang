% --- data1.dat ----------------------------
{person, "joe", "armstrong",
  [{occupation, programmer},
  {favoriteLanguage, erlang}]}.

{cat, {name, "zorro"},
  {owner, "joe"}}.
% ------------------------------------------

% > Reading All the Terms in the File:

file:consult("data1.dat").
%%{ok,[{person,"joe", "armstrong",
%%      [{occupation,programmer},{favoriteLanguage,erlang}]},
%%   {cat,{name,"zorro"},{owner,"joe"}}]}


% > Reading the Terms in the File One at a Time:

-spec io:read(IoDevice, Prompt) -> {ok, Term} | {error,Why} | eof
%% Prompt is used only to provide a prompt if we use
%% io:read to read from standard input.

{ok, S} = file:open("data1.dat", read).
%%{ok,<0.36.0>}
io:read(S, '').
%%{ok,{person,"joe", "armstrong",
%%  [{occupation,programmer},{favoriteLanguage,erlang}]}}
io:read(S, '').
%%{ok,{cat,{name,"zorro"},{owner,"joe"}}}
io:read(S, '').
%%eof
file:close(S).


% > we could have implemented file:consult

consult(File) ->
  case file:open(File, read) of
    {ok, S} ->
      Val = consult1(S),
      file:close(S),
      {ok, Val};
    {error, Why} ->
      {error, Why}
  end.

consult1(S) ->
  case io:read(S, '') of
    {ok, Term} -> [Term|consult1(S)];
    eof
      -> [];
    Error
      -> Error
  end.


% > Reading the Lines in a File One at a Time

{ok, S} = file:open("data1.dat", read).
io:get_line(S, '').
%%"{person, \"joe\", \"armstrong\",\n"
io:get_line(S, '').
%%"\t[{occupation, programmer},\n"
..
file:close(S).

% > Reading the Entire File into a Binary
%%  read an entire file into a binary using a single
%%  atomic operation.
file:read_file("data1.dat").
%%{ok,<<"{person, \"joe\", \"armstrong\""...>>}


% > Reading a File with Random Access (seeking)

{ok, S} = file:open("data1.dat", [read,binary,raw]).
file:pread(S, 22, 46). % reads exactly Len bytes
%%{ok,<<"rong\",\n\t[{occupation, programmer},\n\t {favorite">>}
file:pread(S, 1, 10).
%%{ok,<<"person, \"j">>}
file:pread(S, 2, 10).
%%{ok,<<"erson, \"jo">>}
file:close(S).

