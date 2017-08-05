% > Writing a List of Terms to a File

unconsult(File, L) ->
  {ok, S} = file:open(File, write),
  % ~p Pretty-print the argument.
  lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
  file:close(S).

%then:
lib_misc:unconsult("test1.dat",
  [{cats,["zorrow","daisy"]},
  {weather,snowing}]).

file:consult("test1.dat").
%%{ok,[{cats,["zorrow","daisy"]},{weather,snowing}]}


% > Writing Lines to a File

{ok, S} = file:open("test2.dat", write).
io:format(S, "~s~n", ["Hello readers"]).
io:format(S, "~w~n", [123]).
io:format(S, "~s~n", ["that's it"]).
file:close(S).


% > Writing an Entire File in One Operation

-module(scavenge_urls).
-export([urls2htmlFile/2, bin2urls/1]).
-import(lists, [reverse/1, reverse/2, map/2]).

bin2urls(Bin) ->
  gather_urls(binary_to_list(Bin), []).

urls2htmlFile(Urls, File) ->
  file:write_file(File, urls2html(Urls)).

urls2html(Urls) -> [h1("Urls"),make_list(Urls)].
h1(Title) -> ["<h1>", Title, "</h1>\n"].
make_list(L) ->
  ["<ul>\n",
    map(fun(I) -> ["<li>",I,"</li>\n"] end, L),
    "</ul>\n"].

gather_urls("<a href" ++ T, L) ->
  {Url, T1} = collect_url_body(T, reverse("<a href")),
  gather_urls(T1, [Url|L]);
gather_urls([_|T], L) ->
  gather_urls(T, L);
gather_urls([], L) -> L.

collect_url_body("</a>" ++ T, L) -> {reverse(L, "</a>"), T};
collect_url_body([H|T], L)
  -> collect_url_body(T, [H|L]);
collect_url_body([], _)
  -> {[],[]}.


% > Writing to a Random-Access File

{ok, S} = file:open("some_filename_here", [raw,write,binary]).
file:pwrite(S, 10, <<"new">>).
file:close(S).


