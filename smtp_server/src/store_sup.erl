-module(store_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [writer_desc()++ets_store()]}}.

writer_desc() ->
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    {smtps_store, {smtps_store, start_link, []]},
          Restart, Shutdown, Type, [smtps_store]}.

ets_store() ->          
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    {writer, {writer, start_link, []]},
          Restart, Shutdown, Type, [writer]}.