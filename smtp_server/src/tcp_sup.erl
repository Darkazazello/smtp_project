%%%-------------------------------------------------------------------
%%% @author aza <>
%%% @copyright (C) 2012, aza
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2012 by aza <>
%%%-------------------------------------------------------------------
-module(tcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Port).

init(Port) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    AChild = {tcp_server, {tcp_server, start, Port},
	      Restart, Shutdown, Type, [tcp_server]},
    {ok, {SupFlags, [AChild]}}.

