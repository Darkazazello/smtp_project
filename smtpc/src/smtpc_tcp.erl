-module(smtpc_tcp).

-behaviour(gen_event).
-export([add_handler/1]).
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-include("../include/state.hrl").

add_handler({Host, Port, Fsm_Pid}) ->
    gen_event:add_handler(smtpc_tcp, smtpc_tcp, {Host,Port, Fsm_Pid}).

init({Host, Port, Fsm_Pid}) ->
    case init_connection(Host, Port) of
        {ok, Socket} -> 
            {Socket, Fsm_Pid};
            _Any ->
                 error_logger:error_msg("Can't establish a connection to the server at ~p:~p\n", [Host,Port]),
                 {error,lost_connection}
    end.
    

handle_event({send, Data}, {Socket, Fsm_Pid}) ->
    case send(Data, Socket) of
        {error} ->
            terminate("Connection refused/n", []);
        Responce ->
            gen_fsm:send_event(Fsm_Pid, binary:bin_to_list(Responce))
    end;

handle_event(_Event, State) ->
    {ok,State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_connection(Host, Port) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}]) of 
        {ok, Socket} -> Socket;
        _Any ->
            reconnect(Host, Port)
    end.

reconnect(Host, Port) ->
    undefined.

send(Socket,Message) ->
    case gen_tcp:send(Socket, term_to_binary(Message)) of
       {ok, Bin} ->
            loop(Socket);
        {error, _} ->
            error
    end.
loop(Socket) ->
    receive
    {tcp,Socket,Bin} ->
        binary_to_list(Bin)
        after 12000 ->
            error_logger:error_msg("Connection refused\n")
    end.    
