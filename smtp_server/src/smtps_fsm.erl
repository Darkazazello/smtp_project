-module(smtps_fsm).

-behaviour(gen_fsm).

-include("../include/smtp.hrl").

-export([start/1]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4, 
	 smtp_responce/2, error_state/2]).

-define(SERVER, ?MODULE).

start(Pid) ->
    gen_fsm:start(?MODULE, Pid, []).

start_link(Pid) ->
    start(Pid).

init(Pid) ->
    {ok, smtp_responce, {helo, #smtp_state{}, Pid},infinity}.

smtp_responce(Data, {helo, State, Pid}) ->
    case re:run(Data, "^HELO\\s.*", []) of
	{match, _} ->
	    Client = string:sub_string(Data, 5),
	    NextState = State#smtp_state{user=Client},
	    send(Pid, "250 HELO " ++ Client ++ ", I am glad to meet you"),
	    {next_state, smtp_responce, {from, NextState}};
	_Any ->
	    send(Pid, "500 Syntax error, command unrecognised"),
	    {next_state, smtp_responce, State}
    end;

smtp_responce(Data, {from, State, Pid}) ->
    case re:run(Data, "^MAIL\\sFROM:<*>$", []) of
  	  {match, _} ->
  	    Email = string:sub_string(Data,11,string:len(Data)-1),
  	    NextState=State#smtp_state{email=Email},
  	    send(Pid, "250 Ok"),
  	    {next_state, smtp_responce, {rcpt, NextState}};
  	  _Any ->
  	    send(Pid, "500 Syntax error, command unrecognised"),
  	    {next_state, smtp_responce, State}
    end;


smtp_responce("DATA", {State, Pid}) ->
    send(get(Pid, "354 End data with <CR><LF>.<CR><LF>"),
    {next_state, smtp_responce, {mail, State}};

smtp_responce(Data, {rcpt, State, Pid}) ->
    case re:run(Data, "^RCPT\\sTO:<.*>$", []) of  
       {match, _} ->
         Rcpt=string:sub_string(Data,10,string:len(Data)-1), 
         Recipients=State#smtp_state.rcpt ++ [Rcpt],
         NextState=State#smtp_state{rcpt=Recipients},
         send(Pid, "250 Ok"),
         {next_state, smtp_responce, {rcpt, NextState}};
      _Any ->
         send(get(Pid, "500 Syntax error, command unrecognised"),
	 {next_state, smtp_responce, State}
     end;


smtp_responce(Data, {mail, State, Pid})->   
    Mail=State#smtp_state.mail  ++ Data,  
    NextState=State#smtp_state{mail=Mail}, 
    case erlang:list_to_binary(Data) of
  <<13,10,46,10,13>> ->
      ets_store ! {new_mail, State}, 
      send(Pid, "250 Ok: queued as 12345"),
      {next_state,smtp_responce, {quite, NextState}};
  _Any ->
      {next_state,get_data,{mail,NextState}}
    end;

smtp_responce(_, {quite, _, Pid}) ->
    send(Pid, "221 Bye"),
    close_socket(Pid).

error_state(_Event, State) ->
    error_logger:error_message("Terminate FSM. State:~p/n", [State]).

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

send(Pid, Message) ->
    Pid ! {send, Message}.
close_socket(Pid) ->
    Pid ! {close}.
    
