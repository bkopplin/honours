-module(test_lib).

-export([db/0, init_db/1, insert_from_file/3]).

db() ->
		{ok, C} = epgsql:connect(#{host => "localhost",
								  username => "bjarne",
								  password => "password",
								  database => "eneo-test"}),
		 {ok, C}.

init_db(C) ->
	{ok, Sql} = file:read_file(code:priv_dir(eneo) ++ "/sql/init.sql"),
	epgsql:squery(C, Sql).



insert_from_file(C, Application, Filename) ->
		Filepath = code:priv_dir(Application) ++ Filename,
		Sql = file:read_file(Filepath),
		epgsql:squery(C, Sql). 
