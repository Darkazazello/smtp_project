-module(tcp_listener).

-behaviour(gen_server).

-export([start_link/0, start/0, terminate_connections/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, loop/2]).

-include("../include/smtp.hrl").

-record(state, {}).

-define(SERVER, ?MODULE).

-define(OPTS, [binary, {packet, 4}, {reuseaddr, true}, 
        {active, true}]).
-define(PORT,12345).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ets:new(connections, [set,named_table]),
  Pid = spawn(fun() -> connect(nil, ?PORT) end), 	
  ets:insert(connections, {0,Pid}),  
  {ok, #state{}}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({socket, Socket}, State) ->
    {ok, {Address, _Port}} = inet:peername(Socket),
    Ip = inet_parse:ntoa(Address),
    error_logger:info_msg("Server received connection from~p~n", [Ip]),
    case ets:lookup(fsm, {client, Ip}) of
        [] -> 
            error_logger:info_msg("Save client's socket~n"),
	    {ok,FsmPid} = supervisor:start_child(fsm_sup, []),
	    Pid = spawn(fun() -> loop(Socket, FsmPid) end),
	    ets:insert(connections, {FsmPid, Pid}),
            ets:insert(fsm, {{client,Ip}, {fsm, FsmPid}}),
            gen_tcp:controlling_process(Socket, Pid);
        [{{client,Ip},{fsm, FsmPid}}] ->
	    	error_logger:info_msg("Received another connection~n"),
            Pid = spawn(fun() -> loop(Socket, FsmPid) end),
            gen_tcp:controlling_process(Socket, Pid);	
	_Any ->
	    error_logger:error("Something wrong with ets~p~n", [_Any])       
    end,
    {noreply, State};

handle_cast({send, FsmPid, Message}, State) ->
    case ets:lookup(connections, {FsmPid}) of
	[FsmPid, Pid] ->
	    Pid ! {send, Message};
	_Any ->
	    undefined
    end,
    {noreply,State};

handle_cast({close, FsmPid}, State) ->
    case ets:lookup(connections, FsmPid) of
	[FsmPid, Pid] ->
	    Pid ! {close};
	_Any ->
	    undefined
    end,
    {noreply, State}.
	    
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    terminate_connections(ets:tab2list(connections)),
    %%TODO terminate connections
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate_connections([]) ->
    ok;
terminate_connections([{Key,Pid}|B]) ->
    Pid ! {'EXIT', "terminate", self()},
    io:format("Key ~p~n", [Key]),
    terminate_connections(B).

connect(nil, ?PORT) ->
    case gen_tcp:listen(?PORT, ?OPTS) of
	{ok, Listen} ->
	    connect(Listen, ?PORT);
	{'EXIT', _Reason, _Pid} ->
	    error_logger:info_msg("Terminate connect");
	_Any ->
	    error_logger:warning_msg("Can't read socket on server with port ~p~n", [?PORT]),
	    connect(nil, ?PORT)
    end;

connect(Listen, ?PORT) ->
    case gen_tcp:accept(Listen) of
	{ok,Socket} ->
	    gen_server:cast(?SERVER, {socket, Socket}),
	    connect(Listen, ?PORT);
	{'EXIT', _Reason, _Pid} ->
	    error_logger:info_msg("Terminate connect");
	_Any ->
	    error_logger:info_msg("Can't open connection: ~p~n", [_Any,self()]),
	    case gen_tcp:listen(?PORT, ?OPTS) of
		{ok, Listen_} ->
		    connect(Listen_, ?PORT);
		__Any ->
		    error_logger:info_msg("Can't open connection: ~p~n", [__Any,self()])
	    end
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
	    error_logger:info_msg("Send responce ~p~n", [Data]),
            gen_tcp:send(Socket, binary:list_to_bin(Data)),
            inet:setopts(Socket, [{active, once}]),
            loop(Socket, FsmPid);
        {close} ->
            close_socket(Socket),
            error_logger:info_msg("Nice work~n");
	{'EXIT', _Reason, _Pid} ->
	    close_socket(Socket)
        after ?MAX_TIMEOUT ->
	    error_logger:info_report("Server closed socket"),
            gen_tcp:close(Socket)   
    end.

close_socket(Socket) ->
    gen_tcp:close(socket),
    {ok, {Ip, _Port}} = inet:peername(Socket),
    ets:delete(fsm, {client, Ip}).
