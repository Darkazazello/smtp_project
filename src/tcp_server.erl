-module(tcp_server).

-export([tcp_server/0,start_parallel_server/0]).

tcp_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {active, once}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket).

loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    io:format("Server received binary = ~p~n" ,[Bin]),
	    %gen_tcp:send(Socket, term_to_binary("hello")),
	    inet:setopts(Socket, [{active, once}]),
	    loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("Server socket closed~n" )
    end.
start_parallel_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {active, true}]),
    spawn(fun() -> par_connect(Listen) end).
par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    loop(Socket).
