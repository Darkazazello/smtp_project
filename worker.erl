-module(worker).
-export([start_link/0]).

start_link()->
	{ok, register(work, spawn(fun() -> loop() end))}.

loop() ->
	receive
		hello ->
			io:format("Hello~n"),
			loop();
		_Any ->
			io:format("~p~n", [_Any]),
			loop()
	after 
		12000 ->
			io:format("Waiting....~n"),
			loop()
	end.