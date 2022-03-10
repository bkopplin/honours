-module(api_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
		[{timetrap,{seconds,20}}].

init_per_suite(Config) ->
	Database = "eneo-test",
	Root = "../../../..",
	os:cmd("createdb " ++ Database),
	os:cmd("psql -d " ++ Database ++ " -f " ++ Root ++ "/sql/init.sql"),
	% TODO find better solution than using env vars, e.g. config files 
	os:putenv("PG_DATABASE", Database),
	os:putenv("PG_USER", "bjarne"),
	os:cmd(Root ++ "/_build/default/rel/eneo/bin/eneo daemon"),
	ibrowse:start(),
	[{db, Database}|Config].

end_per_suite(Config) ->
	os:cmd("dropdb " ++ proplists:get_value(db, Config)),
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
						
