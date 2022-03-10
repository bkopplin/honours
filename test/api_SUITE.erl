-module(api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(ROOM1, "!t1:localhost").
-define(TOKEN1, "sj3s90324,c9s032sdf239d324").

suite() ->
		[{timetrap,{seconds,20}}].

init_per_suite(Config) ->
	os:putenv("PG_DATABASE", "eneo-test"),
	os:putenv("PG_USER", "bjarne"),
	os:putenv("PG_PASSWORD", "password"),
	application:ensure_all_started(eneo),
	ibrowse:start(),
	Conn = test_lib:mock_db(),
	[{conn, Conn}|Config].

end_per_suite(Config) ->
	application:stop(eneo),
	ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	ok.

init_per_testcase(_TestCase, Config) ->
	ct:pal("proplist: ~w", [proplists:get_value(conn, Config)]),
	test_lib:insert_from_file(proplists:get_value(conn, Config), eneo, "/sql/init.sql"),
	Config.

end_per_testcase(_TestCase, _Config) ->
	ok.

groups() ->
	[].

all() ->
	[
	 get_messages, 
	 valid_message_case
	].

get_messages(Config) ->
	ct:comment("connected to API"),
	{ok, "200", _, Body} = ibrowse:send_req("http://127.0.0.1:8080/rooms/!r1:localhost/messages", [], get).

valid_message_case(Config) ->
	{ok, "200", _, _} = send_put("/rooms/~s/send/m.room.message/~saccess_token=~s", [?ROOM1, "1", ?TOKEN1]).


%%% ------------------------
%%% Internal Helper Functions
%%% -------------------------

send_get(Url, Args) -> send_http(Url, Args, get).
send_post(Url, Args) -> send_http(Url, Args, post).
send_put(Url, Args) -> send_http(Url, Args, put).

send_http(Url, Args, Method) ->
		ibrowse:send_req(baseurl() ++ io_lib:format(Url, Args), [], Method).



baseurl() ->
	"http://127.0.0.1:8080".
