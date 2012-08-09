-module(store_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(BufferSize) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, BufferSize).

init(BufferSize) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {smtps_store, {smtps_store, start_link, BufferSize},
	      Restart, Shutdown, Type, [smtps_store]},
    {ok, {SupFlags, [AChild]}}.
