./rebar clean
./rebar compile
erl -pa ebin/*  -boot start_sasl -s smpt_server
