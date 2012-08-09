-module(smtpc_fsm).

-behaviour(gen_fsm).

-include("../include/state.hrl").

-export([start_link/1, start/1]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4,cmd_client/2,error_state/2]).

start_link(State) ->
    gen_fsm:start_link(?MODULE, State, []).

start(State) ->
    gen_fsm:start(?MODULE, State, []).

init(State) ->
    {ok, cmd_client, {helo,State}}.

cmd_client(Event, {helo,State}) ->
    case send("HELO " ++ State#state.my_host) of
	{ok,_} ->
	    {next_state, cmd_client, {from, State}};
	_Any ->
	    {next_state, cmd_client, {helo,State}}
     end;

cmd_client(Responce, {from, State})  ->
    Template = "250 Hello"++State#state.my_host ++", I am glad to meet you",
    case Responce of
	 Template ->
	    Message = "MAIL FROM:<" ++  State#state.from ++ ">",
	    case send(Message) of
		{ok,_} ->
		    {next_state,cmd_client,{rcpt,State}};
		_Any ->
		    {next_state, error_state, State}
	    end;
	
	_Any ->
	    {next_state, error_state, State}
    end;
cmd_client(Responce, {rcpt,State}) ->
    case Responce of
	"250 Ok" ->
	    case State#state.rcpt of
		[A] ->
		    send("RCPT TO:<" + A + ">"),
		    NewState = State#state{rcpt=[]},
		    {next_state, cmd_client, {data,NewState}};
		[A|B] ->
		    send("RCPT TO:<" + A + ">"),
		    NewState = State#state{rcpt=B},
		    {next_state, cmd_client, {rcpt,NewState}};
		[] ->
		    {next_state, error_state, State}
	    end;
	_Any ->
	    {next_state, error_state, State}
    end;
cmd_client(Responce, {mail,State}) ->
    case Responce of
	"354 End data with <CR><LF>.<CR><LF>" ->
	    case State#state.mail of
		[A] ->
		    send(A),
		    {next_state, cmd_client, {quit, State}};
		[A|B] ->
		    send(A),
		    NewState=State#state{mail=B},
		    {next_state, cmd_client, {mail,NewState}};
		[] ->
		    {next_state, error_state, State}
	    end
    end;
cmd_client(Responce, {quit, State}) ->
    case re:match(Responce, "^250\\sOk") of
	{match, _ , _} ->
	    send("QUIT"),
	    {stop, normal, State};
	_Any ->
	    {next_state, error_state, State}
    end;
cmd_client(Responce, {data,State}) ->			    
    case Responce of
	"250 Ok" ->
	    send("DATA"),
	    {next_state, cmd_client, {mail,State}};
    _Any ->
	    {next_state, error_state, State}
    end;

cmd_client(_Event,State) ->
    %write to log
    error_logger:error_msg("Unrecognize responce from server:~p\n",[_Event]),
    {next_state, error_state, State}.
		
    
error_state(_Event, State) ->    
    error_logger:error_msg("FSM terminate\n"),
    {stop, error, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->
    ok.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(Message) ->
    gen_event:notify(smtpc_tcp, Message).
