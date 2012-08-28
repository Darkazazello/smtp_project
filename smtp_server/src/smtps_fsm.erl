-module(smtps_fsm).

-behaviour(gen_fsm).

-include("../include/smtp.hrl").

-export([start/0, start_link/0]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4, 
	 smtp_responce/2, error_state/2]).

-define(SERVER, ?MODULE).

start() ->
    gen_fsm:start(?MODULE, [], []).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, smtp_responce, {helo, #smtp_state{}},infinity}.

smtp_responce(Data, {helo, State}) ->
    case re:run(Data, "^HELO\\s.*", []) of
	{match, _} ->
	    Client = string:sub_string(Data, 5),
	    NextState = State#smtp_state{user=Client},
	    send("250 HELO " ++ Client ++ ", I am glad to meet you"),
	    {next_state, smtp_responce, {from, NextState}};
	_Any ->
	    send("500 Syntax error, command unrecognised"),
	    {next_state, smtp_responce, State}
    end;

smtp_responce(Data, {from, State}) ->
    case re:run(Data, "^MAIL\\sFROM:<*>$", []) of
  	  {match, _} ->
  	    Email = string:sub_string(Data,11,string:len(Data)-1),
  	    NextState=State#smtp_state{email=Email},
  	    send("250 Ok"),
  	    {next_state, smtp_responce, {rcpt, NextState}};
  	  _Any ->
  	    send("500 Syntax error, command unrecognised"),
  	    {next_state, smtp_responce, State}
    end;


smtp_responce("DATA", {rcpt, State}) ->
    send("354 End data with <CR><LF>.<CR><LF>"),
    {next_state, smtp_responce, {mail, State}};

smtp_responce(Data, {rcpt, State}) ->
    case re:run(Data, "^RCPT\\sTO:<.*>$", []) of  
       {match, _} ->
         Rcpt=string:sub_string(Data,10,string:len(Data)-1), 
         Recipients=State#smtp_state.rcpt ++ [Rcpt],
         NextState=State#smtp_state{rcpt=Recipients},
         send("250 Ok"),
         {next_state, smtp_responce, {rcpt, NextState}};
      _Any ->
         send("500 Syntax error, command unrecognised"),
	 {next_state, smtp_responce, State}
     end;

smtp_responce(Data, {mail, State})->   
    Mail=State#smtp_state.mail  ++ Data,  
    NextState=State#smtp_state{mail=Mail}, 
    case erlang:list_to_binary(Data) of
  <<13,10,46,10,13>> ->
      gen_server:cast(file_worker,{new_mail, State}), 
      send("250 Ok: queued as 12345"),
      {next_state,smtp_responce, {quite, NextState}};
  _Any ->
      {next_state,get_data,{mail,NextState}}
    end;

smtp_responce(_, {quite, _}) ->
    send("221 Bye"),
    close_socket().

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

send(Message) ->
     gen_server:cast({send, self(), Message}).
close_socket() ->
    gen_server:cast({close, self()}).
    
