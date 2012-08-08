-module(tcp_client).
-export([client/1]).
client(Str) ->
    receive
	_Any -> {ok}
    after 1200 ->
	    {ok, Socket} = gen_tcp:connect("localhost" , 2345,
				   [binary, {packet, 4}]),
	    ok = gen_tcp:send(Socket, term_to_binary(Str)),
	    client(Str)
    end.
    % receive
    % 	{tcp,Socket,Bin} ->
    % 	    io:format("Client received binary = ~p~n" ,[Bin]),
    % 	    Val = binary_to_term(Bin),
    % 	    io:format("Client result = ~p~n" ,[Val]),
    % 	    gen_tcp:close(Socket)
   
