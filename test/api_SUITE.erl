-module(api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
		[{timetrap,{seconds,20}}].

init_per_suite(Config) ->
	os:putenv("PG_DATABASE", "eneo-test"),
	os:putenv("PG_USER", "bjarne"),
	os:putenv("PG_PASSWORD", "password"),
	application:ensure_all_started(eneo),
	ibrowse:start(),
	Config.

end_per_suite(Config) ->
	application:stop(eneo),
	ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	ok.

init_per_testcase(_TestCase, Config) ->
	os:cmd("psql -d eneo-test -f " ++ code:priv_dir(eneo) ++ "/sql/init.sql"),
	Config.

end_per_testcase(_TestCase, _Config) ->
	ok.

groups() ->
	[].

all() ->
	[app_running_case].

app_running_case(Config) ->
	ct:comment("connected to API"),
	{ok, "200", _, Body} = ibrowse:send_req("http://127.0.0.1:8080/rooms/!r1:localhost/messages", [], get).



%%% ------------------------
%%% Internal Helper Functions
%%% -------------------------

create_db(Datname) ->
		{ok, C} = epgsql:connect(#{
					username => os:getenv("PG_USERNAME"),
					password => "password",
					database => bjarne,
					host => "localhost"}),
		epgsql:equery(C, "CREATE DATABASE IF NOT EXISTS $1;", [Datname]), 
		epgsql:close(C).
						
