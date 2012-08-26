-module(writer).

-export([start_link/0]).

-include("../include/smtp.hrl").

-define(SERVER, ?MODULE).

-define(ROOT, smtp_server:get_root()).

start_link() ->
    Pid = spawn(fun() -> loop() end),
    register(writer, Pid),
    {ok, Pid}.

loop() ->
    receive 
        {write_files, TableName} ->
            write_files(TableName),
            loop();
        exit ->
            io:format("Exit");
        _Any ->
            loop()
    end.

write_files(Table) ->
    List = ets:table2list(Table),
    ets:delete_all_objects(data),
    view_list(List).

generate_name(User, Folder) ->
    Count = ets:lookup(users, User) + 1,
    ets:insert(users, {User, Count}),
    ?ROOT ++"/" ++ Folder ++ "/" ++ Count ++ ".mail".

view_list(List) ->
    lists:foreach(fun({User, Record}) -> write_file({generate_name(User, Record#smtp_state.host), Record#smtp_state.mail}) end, List).
  
write_file({File, Data}) ->
    case file:open(File, write) of 
	{ok,S} ->
        try
            io:format(S,"~n~s", [Data])
        catch 
            _Any ->
                ets:insert(backup, {File, Data})
        end,                  
	    file:close(S);
	_Any ->
	    ets:insert(backup, {File, Data}),
        error_logger:error_message("Can't save mail, ~p", [File])
    end.
