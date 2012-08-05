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
    smtp_sv:start_link().
    %supervisor:start_link({local, smtpd_sup}, smtpd_sup, [25, smtpd_fsm]).

stop(_S) ->
    ok.
