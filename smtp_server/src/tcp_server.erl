%% @todo add superviser
-module (tcp_server).
-export([start/1]).

start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
        {active, once}]),
    %%error_log:info_report("Starting tcp server..."),
    supervisor:start_child(tcp_listener_sup, []).
    
send_responce(Socket, Data) ->
    error_logger:info_report("Send responce ~p", [Data]),
    gen_tcp:send(binary:list_to_bin(Data)),
    inet:setopts(Socket, [{active, once}]).

close_socket(Socket) ->
    gen_tcp:close(socket),
    {ok, {Ip, Port}} = inet:peername(Socket),
    ets:delete(fsm, {client, Ip}).
