-module(tcps_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
start_link(Port) ->
    supervisor:start_link({local, tcps_sup}, ?MODULE, [Port]);

start_link() ->
    supervisor:start_link({local, tcps_sup}, ?MODULE, [25]).

init([Port]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {tcps_sup, {tcp_server, start_link, [Port]},
	      Restart, Shutdown, Type, [tcp_server]},

    {ok, {SupFlags, [AChild]}}.
