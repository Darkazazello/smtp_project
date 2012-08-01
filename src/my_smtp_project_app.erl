-module(my_smtp_project_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    my_smtp_project_sup:start_link().

stop(_State) ->
    ok.
