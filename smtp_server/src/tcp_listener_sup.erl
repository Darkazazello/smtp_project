-module(tcp_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Listen) ->
    supervisor:start_link({local, tcp_listener_sup}, ?MODULE, [Listen]).

init([Listen]) ->
    RestartStrategy = simple_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {tcp_listener, {tcp_listener, start_link, [Listen]},
	      Restart, Shutdown, Type, [tcp_listener]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
