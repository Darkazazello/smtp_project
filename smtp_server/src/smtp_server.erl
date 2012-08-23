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
    sv_sup:start_link().


get_port() ->
    {ok, Port}=application:get_env(smtp_server, port),
    Port.

get_buffer() ->
   {ok, BufferSize} = application:get_env(smtp_server, buffer_size),
   BufferSize.

get_root() ->
    {ok,Root} = application:get_env(smtp_server, root),
    Root.

stop(_S) ->
    ok.
