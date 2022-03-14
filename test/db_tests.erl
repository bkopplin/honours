-module(db_tests).

-import(db, [table_to_list/2, zip_col_row/2, insert_message/5]).

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

insert_message_test_() ->
		[
		 {"test empty database working",
			?setup(fun empty_database/1)},
		 {"insert message into room",
		 	?setup(fun insert_message_into_room/1)},
		 {"insert message into non-existing room",
		  	?setup(fun insert_message_nonexisting_room/1)},
		 {"insert two messages with different txdId's",
		 	?setup(fun insert_message_different_txdid/1)},
		 {"insert two messages with same txdId's",
			?setup(fun insert_message_same_txdid/1)}
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
		 {ok, Sql} = file:read_file(code:priv_dir(eneo) ++ "/sql/init.sql"),
		 A = epgsql:squery(C, Sql),
		 ?debugFmt("A: ~p", [A]),
		 C.

test_stop(C) ->
		epgsql:close(C).

%% -------------------------
%% Test Helper Functions
%% -------------------------

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

empty_database(C) ->
 	?_assertMatch({ok, _, []}, epgsql:squery(C, "SELECT 1 FROM events;")).

insert_message_into_room(C) ->
	?debugMsg("insert_message_into_room"),
	mock_create_event(C),
	InsertResult = insert_message(C, <<"{}">>, "!test:localhost", "@tom:localhost", "txd1"),
	Events = epgsql:equery(C, "SELECT content FROM events WHERE type='m.room.message';"),
	?_assertMatch({ok, _, [{<<"{}">>}]}, Events).

insert_message_nonexisting_room(C) ->
	?_assertMatch({error, unknown_room}, insert_message(C, <<"foo">>, "!notexisting:localhost", "@tom:localhost", "txd1")).

insert_message_different_txdid(C) ->
	mock_create_event(C),
	Content1 = <<"{\"foo\":1}">>,
	Content2 = <<"{\"bar\":2}">>,
	insert_message(C, <<"{}">>, "!test:localhost", "@tom:localhost", "txd1"),
	insert_message(C, <<"{}">>, "!test:localhost", "@tom:localhost", "txd2"),
	?_assertMatch({ok, _, [{<<"2">>}]} , epgsql:squery(C, "SELECT COUNT(event_id) FROM events WHERE type = 'm.room.message';")).

insert_message_same_txdid(C) ->
	mock_create_event(C),
	insert_message(C, <<"{}">>, "!test:localhost", "@tom:localhost", "txd1"),
	insert_message(C, <<"{}">>, "!test:localhost", "@tom:localhost", "txd1"),
	?_assertMatch({ok, _, [{<<"1">>}]} , epgsql:squery(C, "SELECT COUNT(event_id) FROM events WHERE type = 'm.room.message';")).

%% @doc Generates an epgsql column for testing purposes.
%% Takes in the columns name and type and sets the remaining values to default values.
%% @end

-spec generate_column(Name :: binary(), Type :: atom()) -> #column{}.

generate_column(Name, Type) ->
	#column{name = Name,type = Type,oid = 3802,
        size = -1,modifier = -1,format = 0,table_oid = 16619,
        table_attr_number = 1}.

%% @doc inserts a mock m.room.create event.
%% db:insert_message assumes that a room creation event already exists. It has to be inserted before working with insert 

mock_create_event(C) ->
	epgsql:equery(C, "INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES ('{\"creator\": \"@tom:localhost\",\"room_version\": \"6\"}', '0', 1635938428328, '!test:localhost', '@tom:localhost', 'm.room.create', '{\"age\": 7773042179}', '', 1);").
