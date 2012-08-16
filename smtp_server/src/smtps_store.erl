-module(smtps_store).

-behaviour(gen_event).
-include("../include/smtp.hrl").

-export([add_handler/1]).


-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]). 

add_handler(BufferSize) ->
    gen_event:add_sup_handler(smpts_store, smtps_store, BufferSize).

init(BufferSize) ->
    {ok, BufferSize}.

handle_event({new_mail, Mail}, State) ->
    ets:insert(mail, {Mail#smtp_state.user, Mail}),
    Count = ets:info(mail,size),
    if 
        Count >= State ->
            gen_event:notify(file_writer);
	true ->
	    error_logger:info_msg("Add new Mail to cache\n")
    end,    
{ok, State};

handle_event(_Event, State) ->
    {ok,State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
