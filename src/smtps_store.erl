%%%-------------------------------------------------------------------
%%% @author aza <>
%%% @copyright (C) 2012, aza
%%% @doc
%%%
%%% @end
%%% Created :  5 Aug 2012 by aza <>
%%%-------------------------------------------------------------------
-module(smtps_store).

-behaviour(gen_event).
-include("../include/smtp.hrl").
%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).
init([]) ->
    etc:new(data, [set,duplicate_bag]),
    {ok, 1}.

handle_event({new_mail, Mail}, State) ->
    etc:insert(Mail#smtp_state.user, Mail),
    NewState = State + 1,
    case NewState of
	NewState >= ?MAX_RECORDS ->
	    %notify file worker to write record
	    case write_to_files() of
		ok ->
		    NewState = 0,
		    etc:delete_all_objects(data);
		error ->
		    %notify supervisor
		    ok
	    end;
	_Any ->
	    ok
    end,
    {ok, NewState};

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
