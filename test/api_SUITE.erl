-module(api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
		[{timetrap,{seconds,20}}].

init_per_suite(Config) ->
		os:putenv("PG_PASSWORD", "password"),
		os:cmd("../../../default/rel/eneo/bin/eneo daemon"),
		ibrowse:start(),
		Config.

end_per_suite(_Config) ->
		os:cmd("../../../default/rel/eneo/bin/eneo stop"),
		ok.

init_per_group(_GroupName, Config) ->
		Config.

end_per_group(_GroupName, _Config) ->
		ok.

init_per_testcase(_TestCase, Config) ->
		Config.

end_per_testcase(_TestCase, _Config) ->
		ok.

groups() ->
		[].

all() ->
		[app_running_case].

app_running_case(Config) ->
		{ok, "200", _, Body} = ibrowse:send_req("http://127.0.0.1:8080/rooms/!r1:localhost/messages", [], get),
		ct:pal(Body),
		ok.

rooms_messages_case(Config) ->
	ct:pal("~p", [application:info()]),
	ct:pal("eneo_sup: ~p", [whereis(eneo_sup)]),
	{ok, "200", _, Body} = ibrowse:send_req("http://localhost:8080/rooms/!r1:localhost/messages", [], get),
	ok.

