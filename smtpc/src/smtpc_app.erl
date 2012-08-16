-module(smtpc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, stop/1, start/2, state/0]).
-include("../include/state.hrl").
%% ===================================================================
%% Application callbacks
%% ===================================================================
start(Arg1, Arg2) ->
    start().
start() ->
    {ok, Host} =  application:get_env(smtpc, host),
    {ok, Port} = application:get_env(smtpc, port),
    State = state(),
    {ok, Pid} = smtpc_fsm:start(State),
    gen_event:start({local, smtpc_tcp}),
    smtpc_tcp:add_handler({Host, Port, Pid}),
    gen_fsm:sync_send_event(Pid, start),
    {ok, Pid}.

stop(_State) ->
    ok.

state() ->
    #state{my_host="wwww.localhost.com", from="a@localhost.com",
		   rcpt=["a1@localhost.com", "a2@localhost.com"],
		   mail=["From: \"Bob Example\" <bob@example.org>",
			 "To: \"Alice Example\" <alice@example.com>",
			 "Date: Tue, 15 January 2008 16:02:43 -0500",
			 "Subject: Test message",<<13,10,46,10,13>>]}.
