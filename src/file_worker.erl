-module(file_worker).

%-export([write_files/2]).

-compile(export_all).

write_files(Table) ->
    List = etc:table2list(Table),
    view_list(List).

%%Pid = spawn(fun() -> write_file(List)).

view_list(List) ->
    lists:foreach(fun({File, Data}) -> write_file({File, Data}) end, List).
  


write_file({File, Data}) ->
    case file:open(File, write) of 
	{ok,S} ->
	    io:format(S,"~n~s", [Data]),
	    file:close(S);
	_Any ->
	    %%create new etc table with failed list of files data.
	    {error, _Any}
    end.
