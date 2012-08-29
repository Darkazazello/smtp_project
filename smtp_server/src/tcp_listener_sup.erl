-module(tcp_listener_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, tcp_listener_sup}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent ,
    Shutdown = 2000,
    Type = worker,

    AChild = {tcp_listener, {tcp_listener, start_link, []},
	      Restart, Shutdown, Type, [tcp_listener]},

    {ok, {SupFlags, [AChild]}}.
