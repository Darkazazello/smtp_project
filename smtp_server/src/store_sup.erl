-module(store_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    {ok, {SupFlags, [store(1,1)]}}.

store(A,B) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    {file_worker, {file_worker, start_link, []},
          Restart, Shutdown, Type, [file_worker]}.
