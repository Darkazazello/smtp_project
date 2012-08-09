-module(file_worker).

-behaviour(gen_event).

-export([add_handler/1]).

-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include("../include/smtp.hrl").

-define(SERVER, ?MODULE).

add_handler(RootPath) ->
    gen_event:add_sup_handler(file_worker, file_worker, [RootPath]).

init(Root) ->
    put(root, Root),
    {ok, []}.

handle_event(write_files, State) ->
    write_files(data),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write_files(Table) ->
    List = etc:table2list(Table),
    etc:delete_all_objects(data),
    view_list(List).

generate_name(User, Folder) ->
    Count = etc:lookup(users, User) + 1,
    etc:insert(users, {User, Count}),
    get(root)++"/" ++ Folder ++ "/" ++ Count ++ ".mail".

view_list(List) ->
    lists:foreach(fun({User, Record}) -> write_file({generate_name(User, Record#smtp_state.host), Record#smtp_state.mail}) end, List).
  
write_file({File, Data}) ->
    case file:open(File, write) of 
	{ok,S} ->
	    io:format(S,"~n~s", [Data]),
	    file:close(S);
	_Any ->
	    etc:insert(backup, {File, Data}),
        error_logger:error_message("Can't save mail, ~p", [File])
    end.
