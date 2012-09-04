-module(smtpc_fsm).

-behaviour(gen_fsm).

-include("../include/state.hrl").

-export([start_link/1, start/1, send_event/1]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4,cmd_client/2,error_state/2]).

-export([init_connection/2]).
-define(ATTEMPTS, 3).
-define(OPTS, [binary, {packet, 4}, {reuseaddr, true}]).
-define(SERVER, ?MODULE).
-define(MAX_TIMEOUT, 120000).
-define(HOST, "localhost").
-define(PORT, 12345).

start_link(State) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, State, []).

start(State) ->
    gen_fsm:start({local, ?SERVER}, ?MODULE, State, []).

send_event(Event) ->
    gen_fsm:send_event(?MODULE, Event).

init(State) ->
    {ok,Socket} = init_connection(?HOST, ?PORT),
    Pid = spawn_link(fun() -> loop(Socket) end),
    register(sender, Pid),
    gen_tcp:controlling_process(Socket, Pid),
    {ok, cmd_client, {helo,State, Pid}}.

cmd_client(_Event, {helo,State, Pid}) ->
    Pid ! {send,"HELO " ++ State#state.my_host},
    {next_state, cmd_client, {from, State, Pid}};

cmd_client(Responce, {from, State, Pid})  ->
    Template = "250 HELO "++State#state.my_host ++", I am glad to meet you",
    case Responce of
	 Template ->
	    Message = "MAIL FROM:<" ++  State#state.from ++ ">",
	    Pid ! {send, Message},
	    {next_state,cmd_client,{rcpt,State, Pid}};
	_Any ->
	    {next_state, error_state, State}
    end;
cmd_client("250 Ok", {rcpt,State, Pid}) ->
    case State#state.rcpt of
	[A] ->
	    Pid ! {send, "RCPT TO:<" ++ A ++ ">"},
	    NewState = State#state{rcpt=[]},
	    {next_state, cmd_client, {data, NewState, Pid}};
	[A|B] ->
	    Pid ! {send, "RCPT TO:<" + A + ">"},
	    NewState = State#state{rcpt=B},
	    {next_state, cmd_client, {rcpt,NewState, Pid}};
	[] ->
	    {next_state, error_state, State}
    
    end;
cmd_client("354 End data with <CR><LF>.<CR><LF>", {mail,State, Pid}) ->
    case State#state.mail of
	[A] ->
	    Pid ! {send, A},
	    {next_state, cmd_client, {quit, State, Pid}};
	[A|B] ->
	    Pid ! {send, A},
	    NewState=State#state{mail=B},
	    {next_state, cmd_client, {mail,NewState, Pid}};
	[] ->
	    {next_state, error_state, State}
    end;
    
cmd_client("250 Ok", {quit, State, Pid}) ->
    Pid ! {send, "QUIT"},
    {stop, normal, State};

cmd_client("250 Ok", {data,State, Pid}) ->			    
    Pid ! {send, "DATA"},
    {next_state, cmd_client, {mail,State, Pid}};

cmd_client(_Event,State) ->
    %write to log
    error_logger:error_msg("Unrecognize responce from server:~p\n",[_Event]),
    {next_state, error_state, State}.
		
    
error_state(_Event, State) ->    
    error_logger:error_msg("FSM terminate\n"),
    sender ! close,
    {stop, error, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->
    sender ! close,
    ok.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

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
    gen_fsm:notify(smtpc_tcp, {stop, "Couldn't connect to server"}),
    {error};

reconnect(Host, Port, Count) ->
    case gen_tcp:connect(Host, Port, ?OPTS) of 
        {ok, Socket} -> {ok, Socket};
        _Any ->
            reconnect(Host, Port, Count - 1)
    end.
    
loop(Socket) ->
    put(socket, Socket),
    loop().

loop() ->
    receive
	{tcp, _Socket, Bin} ->
	    error_logger:info_msg("Received message ~p~n", [binary:bin_to_list(Bin)]),
	    gen_fsm:send_event(?SERVER,binary:bin_to_list(Bin)),
	    loop();
	{send, Message} ->
	    error_logger:info_msg("Send message ~p~n", [Message]),
	    gen_tcp:send(get(socket), term_to_binary(Message)),
	    loop();
	close ->
	    gen_tcp:close_socket(get(socket)),
	    error_logger:info_msg("Close socket");
	_Any ->
	    error_logger:error_msg("Unrecognised signal from fms ~p~n", [_Any]),
	    gen_fsm:send_event(?SERVER, error)
    after ?MAX_TIMEOUT ->
	    error_logger:error_msg("Connection refused\n"),
	    gen_fsm:send_event(?SERVER, timeout)	
	    
    end.    

