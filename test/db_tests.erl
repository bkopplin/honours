-module(db_tests).

-import(db, [table_to_list/2, zip_col_row/2]).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), 
		{setup,
		 fun test_start/0,
		 fun test_stop/1,
		F 
		}).


%% -------------------
%% Test Descriptions
%% -------------------

zip_test_() ->
		[
		 {"empty values", ?_assertEqual([], zip_col_row([], []))},
		 {"invalid args", ?_assertError(function_clause, zip_col_row([],[foo]))},
	 	 {"missing arg", ?_assertError(function_clause, zip_col_row([generate_column(<<"foo">>, text)], []))},
		 {"zip one column", ?_assertEqual([{<<"event_id">>, <<"123">>}], zip_col_row([generate_column(<<"event_id">>, text)], {<<"123">>}))},
		 {"zip multiple columns", zip_multiple_columns()}
		].

message_test_() ->
		[
		 ?setup(fun test_empty_database/1),
		 ?setup(fun test_insert_message/1)
		].

%% ---------------------
%% Test Setup functions
%% ---------------------

test_start() ->
		?debugMsg("setup database"),
		{ok, C} = epgsql:connect(#{host => "localhost",
								  username => "bjarne",
								  password => "password",
								  database => "eneo-test"}),
		 {ok, Sql} = file:read_file("/home/bjarne/code/eneo/priv/sql/init.sql"),
		 epgsql:squery(C, Sql),
		 C.

test_stop(C) ->
		epgsql:close(C).

%% -------------------------
%% Test Helper Functions
%% -------------------------

test_empty_database(C) ->
		 ?_assertMatch({ok, _, []}, epgsql:squery(C, "SELECT event_id FROM events;")).

test_insert_message(C) ->
	epgsql:equery(C, "INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES ('{\"body\": \"hello test\", \"msgtype\": \"m.text\"}',   '123', 1638547064954, '!test:localhost', '@tester:localhost', 'm.room.message', '{\"age\": 4046466692}', NULL, 11);"),
	Events = epgsql:equery(C, "SELECT event_id FROM events;"),
	?_assertMatch({ok, _, [{<<"123">>}]}, Events).


zip_multiple_columns() ->
	Expected = [
		{<<"content">>, #{<<"body">> => <<"hello world">>, <<"msgtype">> => <<"m.text">>}},
	    {<<"event_id">>, <<"123">>},
	    {<<"origin_server_ts">>, 1638547064954}
	], 
	Cols = [ 
		generate_column(<<"content">>, jsonb),
		generate_column(<<"event_id">>, text),
		generate_column(<<"origin_server_ts">>, int8)
 	], 
	Rows =	[<<"{\"body\": \"hello world\", \"msgtype\": \"m.text\"}">>, <<"123">>,1638547064954],
	?_assertEqual(Expected, zip_col_row(Cols, Rows)).


%% @doc Generates an epgsql column for testing purposes.
%% Takes in the columns name and type and sets the remaining values to default values.
%% @end

-spec generate_column(Name :: binary(), Type :: atom()) -> #column{}.

generate_column(Name, Type) ->
	#column{name = Name,type = Type,oid = 3802,
        size = -1,modifier = -1,format = 0,table_oid = 16619,
        table_attr_number = 1}.
