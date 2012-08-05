start_parallel_server() ->
{ok, Listen} = gen_tcp:listen(...),
spawn(fun() -> par_connect(Listen) end).
par_connect(Listen) ->
{ok, Socket} = gen_tcp:accept(Listen),
spawn(fun() -> par_connect(Listen) end),
loop(Socket).
loop(..) ->
