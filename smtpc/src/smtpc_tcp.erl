-module(smtpc_tcp).

-behaviour(gen_event).

-export([add_handler/1]).
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-include("../include/state.hrl").

-define(SERVER, ?MODULE). 
-define(ATTEMPTS, 3).
-define(OPTS, [binary, {packet, 4}]).

add_handler({Host, Port, Fsm_Pid}) ->
    gen_event:add_handler(smtpc_tcp, smtpc_tcp, {Host,Port, Fsm_Pid}).

init({Host, Port, Fsm_Pid}) ->
    case init_connection(Host, Port) of
        {ok, Socket} -> 
            {Socket, Fsm_Pid};
            _Any ->
                 error_logger:error_msg("Can't establish a connection to the server at ~p:~p\n", [Host,Port]),
                 terminate(error_connection, [])	    
    end.
    
handle_event({send, Data}, {Socket, Fsm_Pid}) ->
    error_logger:info_report("Received event"),
    case send(Data, Socket) of
        {error} ->
            terminate("Connection refused/n", []);
        Responce ->
	    error_logger:info_msg("Get responce ~p~n", [Responce]),
            gen_fsm:send_event(Fsm_Pid, Responce)
    end;

handle_event({stop, Reason}, State) ->
    error_logger:error_msg("~p~n",[Reason]),
    terminate(Reason, State).

handle_call(_Request, _State) ->
    error.

handle_info(_Info, _State) ->
    error.

terminate(_Reason, _State) ->
    {error, terminate}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_connection(Host, Port) ->
    case gen_tcp:connect(Host, Port, ?OPTS) of 
        {ok, Socket} -> {ok, Socket};
        _Any ->
            reconnect(Host, Port, ?ATTEMPTS)
    end.

reconnect(_,_,0) ->
    gen_event:notify(smtpc_tcp, {stop, "Couldn't connect to server"}),
    {error};

reconnect(Host, Port, Count) ->
    case gen_tcp:connect(Host, Port, ?OPTS) of 
        {ok, Socket} -> {ok, Socket};
        _Any ->
            reconnect(Host, Port, Count - 1)
    end.
    

send(Socket,Message) ->
    case gen_tcp:send(Socket, term_to_binary(Message)) of
       {ok, _Bin} ->
            loop(Socket);
        {error, _} ->
            error
    end.
loop(Socket) ->
    receive
    {tcp,Socket,Bin} ->
        binary:bin_to_list(Bin)
        after 12000 ->
            error_logger:error_msg("Connection refused\n")
    end.    
