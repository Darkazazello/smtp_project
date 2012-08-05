%%%-------------------------------------------------------------------
%%% @author aza <>
%%% @copyright (C) 2012, aza
%%% @doc
%%%
%%% @end
%%% Created :  2 Aug 2012 by aza <>
%%%-------------------------------------------------------------------
-module(gen_smtp_server_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, responce_data/1,stop/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4 ]).

-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    case gen_fsm:start(?MODULE, [], []) of
	{ok, Pid} ->
	     put("Pid", Pid),
	    {ok};
	_Any ->
	    {error, _Any}
    end.

responce_data(Data) ->
    gen_fsm:sync_send_event(get("Pid"), Data).

stop()->
    gen_fsm:send_all_state_event(hello, stopit).

init(Args) ->
    %Initialize socket%
    
    {ok, hello_state, Args}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {next_state, Reply, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->
    ok.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
hello_state({hello, {Client, Host}}, _From, State) ->
    %%dict:store("Client", Client),
    io:format("Send hello message from server~n"),
    case send(Client, "250 Hello relay.example.org, I am glad to meet you") of
	ok ->
	    put("Host", Host),
	    io:format("Send hello message from server"),
	    {next_state, from_state, Host};
	_ ->
	    {next_state, error_state, {Client}}
    end.

from_state({from, From}, _From, State) ->
    %%dict:store("From", From),
    case send(State, "250 OK") of
	ok ->
	    io:format("Getting requestor's mail"),
	    {next_state, rcpt_state, {}};
	_ ->
	    {next_state, from_state, {}}
    end.

rcpt_state({rcpt, Recipient}, _From, State) ->
   % case dict:find("Recipient") of
   %     {ok, Value} ->
   % 	   dict:store("Recipient", lists:append (Value, Recipient));
   %     {error} ->
   % 	   dict:store("Recipient", Recipient)
%   end,
   io:format("Get recipient"), 
   case send(State, "250 OK") of
       ok ->
	   {next_state, chooser_state, {}};
       _Any ->
	   {next_state, from_state, {}}
   end.

chooser_state(Data, _From, State) ->
    case Data of
	{rcpt, Recipient} ->
	    rcpt_state({rcpt, Recipient}, _From, State);
	{data} ->
	    start_data({data}, _From, State);
	{mail_data} ->
	    mail_state(Data, _From, State);
	_Any ->
	    {next_state, error_state, ""}
    end.

start_data({data}, _From, State) ->
    io:format("354 End data with <CR><LF>.<CR><LF>~n"),
    case send(State, "354 End data with <CR><LF>.<CR><LF>") of
	{ok} ->
	    {next_state, mail_state, State};
	_Any ->
	    {next_state, error, State}
    end.    

mail_state({mail, Data}, _From, State) ->
    io:format("Getting mail data...~n"),
    case erlang:list_to_binary(Data) of
	<<13,10,46,10,13>> ->
	    send(State, "250 Ok: queued as 12345"),
	    io:format("Save mail to disk~n"),
	    {next_state, quite_state, State};
	_ ->
	    case dict:find("Mail") of
		{ok, Value} ->
		    dict:store("Mail", lists:append (Value, Data));
		    
		{error} ->
		    dict:store("Recipient", Data)
	    end,
	    {next_state, chooser_state, State}
    end.

quite_state({quite}, _From, State) ->
    io:format("End.~n"),
    send(State, "221 Bye").

send(Recipient, Message) ->
    io:format("Send~n"),
    {ok}.
	
