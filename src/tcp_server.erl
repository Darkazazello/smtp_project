-module (tcp_server).
-export([start/1]).

-define(PORT, 2345).

start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
        {active, once}]),
    spawn(fun() -> par_connect(Listen) end).
    

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    {ok, {Ip, Port}} = inet:peername(Socket),
    
    case etc:lookup(fsm, Ip) of
        [] -> 
            {ok,Pid} = smtp_fsm:start_link(Socket),
            etc:insert(fsm, {{client,Ip}, {fsm, Pid}}),
            loop(Socket, Pid);
        {{client,Ip},{fsm, Pid}} ->
            loop(Socket, Pid)
    end,
    loop(Socket, Pid).
    
loop(Socket, Pid) ->
    receive
        {tcp, Socket, Bin} ->
                gen_fsm:sync_send_event(Pid, Bin),            
            loop(Socket, Pid);
        {tcp_close} ->
            gen_tcp:close(Socket),  
            io:format("Server closed socket")
        after 1200 ->
            gen_tcp:close(Socket)   
    end.


send_responce(Socket, Data) ->
    gen_tcp:send(Data),
    inet:setopts(Socket, [{active, once}]).
