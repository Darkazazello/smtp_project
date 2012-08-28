-module(store_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link([Size,Root]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Size,Root]).

init([Size,Root]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    {ok, {SupFlags, [store([Size,Root])]}}.

store([Size,Root]) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    {file_worker, {file_worker, start_link, [Size, Root]},
          Restart, Shutdown, Type, [file_worker]}.
