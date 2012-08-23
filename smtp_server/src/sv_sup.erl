-module(sv_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(PORT, smtp_server:get_port()).
-define(BUFFER, smtp_server:get_buffer()).

start_link() ->
    supervisor:start_link({local, sv_sup}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Childs=tcp_sup()++store_sup()++fsm_sup(),
    {ok, {SupFlags, [Childs]}}.

tcp_sup() ->
   	Restart = permanent,
   	Shutdown = 2000,
   	Type = supervisor,
	{tcpl_sup, {tcp_listener_sup, start_link, [?PORT]},
    	Restart, Shutdown, Type, [tcp_listener_sup]}.

store_sup() ->
	Restart = permanent,
   	Shutdown = 2000,
   	Type = supervisor,
	{store_sup, {store_sup, start_link, [?BUFFER]},
    	Restart, Shutdown, Type, [store_sup]}.

fsm_sup() ->
	Restart = permanent,
   	Shutdown = 2000,
   	Type = supervisor,
	{fsm_sup, {fsm_sup, start_link, []},
    	Restart, Shutdown, Type, [fsm_sup]}.
