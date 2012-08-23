-module(sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, s1}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Childs=tcp_sup(),
    {ok, {SupFlags, [Childs]}}.

tcp_sup() ->
   	Restart = permanent,
   	Shutdown = 2000,
   	Type = worker,
	{sup, {worker, start_link, []},
    	Restart, Shutdown, Type, [worker]}.
