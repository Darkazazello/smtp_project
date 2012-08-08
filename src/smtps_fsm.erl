%%%-------------------------------------------------------------------
%%% @author aza <>
%%% @copyright (C) 2012, aza
%%% @doc
%%%
%%% @end
%%% Created :  5 Aug 2012 by aza <>
%%%-------------------------------------------------------------------
-module(smtps_fsm).

-behaviour(gen_fsm).

-include("../include/smtp.hrl").

-import(regexp).

%% API
-export([start_link/0, start/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4, 
	 smtp_responce/2, get_data/2, error_state/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_fsm:start(?MODULE, [], []).

init(Args) ->
    etc:new(users, {set}),
    {ok, smtp_responce, #smtp_state{},infinity}.

% smtp_responce({helo, Client}, State) ->
%     io:format("smtp_responce(_Event, State)~p~n", [State]),
%     {next_state, error_state, State}.

smtp_responce({helo, Data}, State) ->
    case regexp:match(Data, "^HELO\\s.*") of
	{match, _, _} ->
	    Client = string:sub_string(Data, 5),
	    NextState = State#smtp_state{user=Client},
	    send("250 HELO " ++ Client ++ ", I am glad to meet you"),
	    {next_state, smtp_responce, NextState};
	_Any ->
	    send("500 Syntax error, command unrecognised"),
	    {next_state, smtp_responce, State}
    end;

smtp_responce({from, Data}, State) ->
    case regexp:match(Data, "^MAIL\\sFROM:<.*>$") of
	{match, _, _} ->
	    Email = string:sub_string(Data,11,string:len(Data)-1),
	    NextState=State#smtp_state{email=Email},
	    send("250 Ok"),
	    {next_state, smtp_responce, NextState};
	_Any ->
	    send("500 Syntax error, command unrecognised"),
	    {next_state, smtp_responce, State}
    end;

smtp_responce({rcpt, Data}, State) ->
    case regexp:match(Data, "^RCPT\\sTO:<.*>$") of 
       {match, _, _} ->
         Rcpt=string:sub_string(Data,10,string:len(Data)-1),
         Recipients=State#smtp_state.rcpt ++ [Rcpt],
         NextState=State#smtp_state{rcpt=Recipients},
         send("250 Ok"),
         {next_state, smtp_responce, NextState};
      _Any ->
         send("500 Syntax error, command unrecognised"),
	 {next_state, smtp_responce, State}
     end;

smtp_responce({data, Data}, State) ->
    case string:equals(Data, "DATA") of
	true ->
	    send("354 End data with <CR><LF>.<CR><LF>"),
	    {next_state, get_data, State};
	_Any ->
	    send("500 Syntax error, command unrecognised"),
	    {next_state, smtp_responce, State}
    end;

smtp_responce({quite}, State) ->
    send("221 Bye"),
    close_socket().

%smtp_responce(_Event, State) ->
%    {next_state, error_state, State}.

error_state(_Event, State) ->
    io:format("Error~n").

get_data(Data, State) ->
    Mail=State#smtp_state.mail  ++ Data,
    NextState=State#smtp_state{mail=Mail},
    case erlang:list_to_binary(Data) of
	<<13,10,46,10,13>> ->
	    case  etc:lookup(users,State#smtp_state.user) of
		[] ->
		    etc:insert(users, {State#smtp_state.user, 1});
		{_, Count} ->
			   etc:insert(users, {State#smtp_state.user, Count + 1})
	    end,
	    gen_event:notify(writer, {new_mail, State}),
	    send("250 Ok: queued as 12345"),
	    {next_state,smtp_responce, NextState};
	_Any ->
	    {next_state,get_data,NextState}
    end.
handle_event(_Event, StateName, State) ->
    io:format("From handle_event~n"),
    smtp_responce({helo,"1"},State).
    %{next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    io:format("From handle_sync event~n"),
    {reply, Reply, StateName, State}.
handle_info(_Info, StateName, State) ->
    io:format("From handle_info~n"),
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->
    io:format("From terminate~n"),
    ok.
code_change(_OldVsn, StateName, State, _Extra) ->
    io:format("From code change~n"),
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send(Message) ->
    io:format("~p~n", [Message]).
%send(Socket, Message) -> 
%    io:format("~p~n", [Message]).

close_socket() ->
    io:format("Closing connection...~n").
