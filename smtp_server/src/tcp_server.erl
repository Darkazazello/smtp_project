-module (tcp_server).
-export([start/1]).

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
            {ok,Pid} = smtp_fsm:start(Socket),
            etc:insert(fsm, {{client,Ip}, {fsm, Pid}}),
            loop(Socket, Pid);
        {{client,Ip},{fsm, Pid}} ->
            loop(Socket, Pid)
    end,
    loop(Socket, Pid).
    
loop(Socket, Pid) ->
    receive
        {tcp, Socket, Bin} ->
                gen_fsm:sync_send_event(Pid, binary:bin_to_list(Bin)),            
            loop(Socket, Pid);
        {tcp_close} ->
            gen_tcp:close(Socket),  
            io:format("Server closed socket")
        after 12000 ->
            gen_tcp:close(Socket)   
    end.

send_responce(Socket, Data) ->
    gen_tcp:send(binary:list_to_bin(Data)),
    inet:setopts(Socket, [{active, once}]).

close_socket(Socket) ->
    gen_tcp:close(socket),
    {ok, {Ip, Port}} = inet:peername(Socket),
    etc:delete(fsm, {client, Ip}).
