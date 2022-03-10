-module(api_test).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("eunit/include/eunit.hrl").

room_t() ->
		{setup,
		 fun() -> io:format("database: ~s, username: ~s~n", [os:getenv("PG_DATABASE"), os:getenv("PG_USERNAME")]) end,
		 fun() -> io:format("cleanup~n") end,
		 [?_assertEqual(1, os:getenv("PG_DATABASE"))]
		}.

