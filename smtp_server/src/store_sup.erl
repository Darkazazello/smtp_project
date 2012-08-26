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
    
    {ok, {SupFlags, [file_writer(Root)]++[ets_store(Size)]}}.

ets_store(Size) ->
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    {smtps_store, {smtps_store, start_link, [Size]},
          Restart, Shutdown, Type, [smtps_store]}.

file_writer(Root) ->          
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    {writer, {writer, start_link, [Root]},
          Restart, Shutdown, Type, [writer]}.
