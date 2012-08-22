-module(smtp_server).
-include("../include/smtp.hrl").

-behaviour(application).



%% Application and Supervisor callbacks
-export([start/2, stop/1]).




%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    %{ok,ListenPort} = application:get_env(,server_smtp_port),
    ets:new(users, {set, named_table, public}),
    ets:new(backup, {set, named_table, public}),
    ets:new(data, [duplicate_bag, named_table, public]),
    ets:new(fsm, [duplicate_bag, named_table, public]),
    {ok,Root} = application:get_env(smtp_server, root),
    {ok, Port} = application:get_env(smtp_server, port),
    {ok, BufferSize} = application:get_env(smtp_server, buffer_size),
    
    tcp_sup:start_link(Port),%start tcp listener%
    store_sup:start_link(BufferSize),%start store event process
    gen_event:start_link(signals),
    file_worker:add_handler(Root),
    smpts_store:add_handler(BufferSize).
    %supervisor:start_link({local, smtpd_sup}, smtpd_sup, [25, smtpd_fsm]).

get_port() ->
    {ok, Port}=application:get_env(smtp_server, port),
    Port.

get_buffer() ->
   {ok, BufferSize} = application:get_env(smtp_server, buffer_size),
   BufferSize.

stop(_S) ->
    ok.
