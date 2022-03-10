-module(test_lib).

-export([mock_db/0, insert_from_file/3]).

mock_db() ->
		{ok, C} = epgsql:connect(#{host => "localhost",
								  username => "bjarne",
								  password => "password",
								  database => "eneo-test"}),
		 {ok, Sql} = file:read_file(code:priv_dir(eneo) ++ "/sql/init.sql"),
		 epgsql:squery(C, Sql),
		 C.

insert_from_file(C, Application, Filename) ->
		Filepath = code:priv_dir(Application) ++ Filename,
		Sql = file:read_file(Filepath),
		epgsql:squery(C, Sql). 
