-module(smtps_store).

-include("../include/smtp.hrl").

-export([start_link/0]).

-define(BUFFER_SIZE, 10000).

start_link() ->
    {ok, register(ets_store,spawn(fun()->loop() end))}.

loop() ->
    receive 
        {new_mail, Mail} ->
            add_data(Mail),
            loop();
        _Any ->
            loop()
    end.

add_data(Mail) ->
    ets:insert(mail, {Mail#smtp_state.user, Mail}),
    Count = ets:info(mail,size),
    if 
        Count >= ?BUFFER_SIZE ->
            writer ! write_files;
    true ->
        error_logger:info_msg("Add new Mail to cache\n")
    end.    
