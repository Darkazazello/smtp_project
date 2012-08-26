-module(sv_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Params) ->
    supervisor:start_link({local, sv_sup}, ?MODULE, Params).

init([Port, Size, Root]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Childs=[fsm_sup()]++[tcp_sup(Port)]++[store_sup([Size,Root])],
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,
    {ok, {SupFlags, Childs}}.

tcp_sup(Port) ->
   	Restart = permanent,
   	Shutdown = 2000,
   	Type = supervisor,
	{tcp_listener_sup, {tcp_listener_sup, start_link, [Port]},
    	Restart, Shutdown, Type, [tcp_listener_sup]}.

store_sup([Size,Root]) ->
	Restart = permanent,
   	Shutdown = 2000,
   	Type = supervisor,
	{store_sup, {store_sup, start_link, [Size,Root]},
    	Restart, Shutdown, Type, [store_sup]}.

fsm_sup() ->
	Restart = permanent,
   	Shutdown = 2000,
   	Type = supervisor,
	{fsm_sup, {fsm_sup, start_link, []},
    	Restart, Shutdown, Type, [fsm_sup]}.
