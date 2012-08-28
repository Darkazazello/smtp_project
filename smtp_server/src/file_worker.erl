-module(file_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("../include/smtp.hrl").

-record(params, {
	  size=undefined,
	  root=undefined}).

start_link([Size, Root]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Size,Root], []).

init([Size,Root]) ->
    {ok, #params{size=Size, root=Root}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({write_files, TableName}, State) ->
    write_files(TableName, State#params.root),
    {noreply, State};
handle_cast({new_mail, Mail}, State) ->
    add_data(Mail, State#params.size).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
write_files(Table, Root) ->
    List = ets:table2list(Table),
    ets:delete_all_objects(data),
    view_list(List, Root).

generate_name(User, Folder, Root) ->
    Count = ets:lookup(users, User) + 1,
    ets:insert(users, {User, Count}),
    Root ++"/" ++ Folder ++ "/" ++ Count ++ ".mail".

view_list(List, Root) ->
    lists:foreach(fun({User, Record}) -> write_file({generate_name(User, Record#smtp_state.host, Root), Record#smtp_state.mail}) end, List).
  
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

add_data(Mail, MaxSize) ->
    ets:insert(mail, {Mail#smtp_state.user, Mail}),
    Count = ets:info(mail,size),
    if 
        Count >= MaxSize ->
            writer ! write_files;
    true ->
        error_logger:info_msg("Add new Mail to cache\n")
    end.    
