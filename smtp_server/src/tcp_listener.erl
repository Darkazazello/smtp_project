-module(tcp_listener).

-include("../include/smtp.hrl")
-export([connect/1, start/1, start_link/1]).

start(Listen) ->
    connect(Listen).

start_link(Listen) ->
    connect(Listen).

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    supervisor:start_child(tcp_listener_sup, []),
    %spawn(fun() -> connect(Listen) end),
    {ok, {Address, Port}} = inet:peername(Socket),
    Ip = inet_parse:ntoa(Address),
    error_logger:info_msg("Server received connection from~p~n", [Ip]),
    case ets:lookup(fsm, {client, Ip}) of
        [] -> 
            {ok,Pid} = smtp_fsm:start(self()),
            ets:insert(fsm, {{client,Ip}, {fsm, Pid}}),
	    error_logger:info_msg("Save client's socket~n"),
            loop(Socket, Pid);
        [{{client,Ip},{fsm, Pid}}] ->
	    error_logger:info_msg("Received another connection~n"),
            loop(Socket, Pid);
	_Any ->
	    error_logger:error("Something wrong with ets~p~n", [_Any])
       
    end.
    
loop(Socket, FsmPid) ->
    receive
        {tcp, Socket, Bin} ->
	        error_logger:info_report("Receive request~p", binary:bin_to_list(Bin)),
                gen_fsm:sync_send_event(FsmPid, binary:bin_to_list(Bin)),            
            loop(Socket, FsmPid);
        {tcp_close} ->
            gen_tcp:close(Socket),  
            error_logger:info_report("Client closed socket");
        {send, Data} ->
            gen_tcp:send(Socket, binary:list_to_bin(Data)),
            inet:setopts(Socket, [{active, once}]),
            loop(Socket, FsmPid);
        {close} ->
            close_socket(Socket),
            error_logger:info_msg("Nice work~n")
        after ?MAX_TIMEOUT ->
	    error_logger:info_report("Server closed socket"),
            gen_tcp:close(Socket)   
    end.

close_socket(Socket) ->
    gen_tcp:close(socket),
    {ok, {Ip, Port}} = inet:peername(Socket),
    ets:delete(fsm, {client, Ip}).