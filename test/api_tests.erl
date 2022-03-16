-module(api_tests).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ROOM1, "!t1:localhost").
-define(TOKEN1, "sj3s90324,c9s032sdf239d324").

-define(setup(F), 
		{setup,
		 fun setup_single/0,
		 fun cleanup_single/1,
		F 
		}).

%% Test setup functions

setup_all() -> 
	os:putenv("PG_DATABASE", "eneo-test"),
	os:putenv("PG_USER", "bjarne"),
	os:putenv("PG_PASSWORD", "password"),
	application:ensure_all_started(eneo),
	ibrowse:start(),
	ok.

cleanup_all(_) ->
		ok.

setup_single() ->
		{ok, C} = epgsql:connect(#{host => "localhost",
								  username => "bjarne",
								  password => "password",
								  database => "eneo-test"}),
		 {ok, Sql} = file:read_file(code:priv_dir(eneo) ++ "/sql/init.sql"),
		 epgsql:squery(C, Sql),
		 C.

cleanup_single(C) ->
		epgsql:close(C).

mock_create_event(C) ->
	epgsql:equery(C, "INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES ('{\"creator\": \"@tom:localhost\",\"room_version\": \"6\"}', '0', 1635938428328, '!test:localhost', '@tom:localhost', 'm.room.create', '{\"age\": 7773042179}', '', 1);").

%%% ------------------
%%% Test Descriptions
%%% ------------------

send_message_test_() ->
		{setup, fun setup_all/0, fun cleanup_all/1,
		 [
		   ?setup(fun eunit1/1),
		   ?setup(fun eunit2/1),
		   ?setup(fun eunit3/1),
		   ?setup(fun test_send_message_correct/1),
		   ?setup(fun test_send_message_nonexisting_room/1),
		   ?setup(fun test_send_message_missing_body/1),
		   ?setup(fun test_send_message_content_field_missing/1)
		 ]
		}.

eunit1(C) ->
		?_assertMatch({ok,_, [{<<"0">>}]}, epgsql:squery(C, "select COUNT(event_id) from Events;")).

eunit2(C) ->
		mock_create_event(C),
		?_assertMatch({ok,_, [{<<"1">>}]}, epgsql:squery(C, "select COUNT(event_id) from Events;")).

eunit3(C) ->
	?_assertMatch({ok,_, [{<<"0">>}]}, epgsql:squery(C, "select COUNT(event_id) from Events;")).

%%% ------------------
%%% Test functions
%%% ------------------

test_send_message_correct(C) ->
	db:insert_create_event(C, <<"@tom:localhost">>, ?ROOM1),
	ReqBody =  #{
       <<"body">> => <<"bar">>,
       <<"msgtype">> => <<"m.text">>
	},
	{ok, Status, _, Body} = 
		send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", 
				  [?ROOM1, "1", ?TOKEN1], put, ReqBody),
	[
	 ?_assertMatch(
		{ok, _, [{<<"2">>}]}, 
		epgsql:squery(C, "SELECT COUNT(event_id) FROM events;")
	   ),
	 ?_assertMatch("200", Status),
	 ?_assertMatch(true, maps:is_key(<<"event_id">>, Body))
	].

test_send_message_nonexisting_room(C) ->
	ReqBody =  #{
       <<"body">> => <<"bar">>,
       <<"msgtype">> => <<"m.text">>
	},
	{"send a message to a nonexisting room", 
	 ?_assertMatch(
	    {ok, "403", _, #{<<"errcode">> := <<"M_FORBIDDEN">>, <<"error">> := <<"Unknown room">> }},
		send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", [<<"!invalid:room">>, "1", ?TOKEN1], put, ReqBody)
	  )}.

test_send_message_missing_body(C) ->
	{ok, Status, _, Body} = send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", [?ROOM1, "1", ?TOKEN1], put, []),
	[
	 ?_assertMatch("400", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_NOT_JSON">>, <<"error">> := <<"Content not JSON.">>}, Body)
	].

test_send_message_content_field_missing(C) ->
	ReqBody =  #{
       <<"body">> => <<"bar">>
	},
	?_assertMatch(
		{ok, "400", _, #{<<"errcode">> := <<"M_UNKNOWN">>}},
		send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", [?ROOM1, "1", ?TOKEN1], put, ReqBody)).

%%% ------------------------
%%% Internal Helper Functions
%%% -------------------------

send_get(Url, Args) -> send_http(Url, Args, get, []).
send_post(Url, Args) -> send_http(Url, Args, post, []).
send_put(Url, Args) -> send_http(Url, Args, put, []).

send_http(Url, Args, Method, Body0) ->
		Body = jiffy:encode(Body0),
		case ibrowse:send_req(baseurl() ++ io_lib:format(Url, Args), [], Method, Body) of
				{ok, Status, ResponseHeaders, []} ->
						{ok, Status, ResponseHeaders, []};
				{ok, Status, ResponseHeaders, ResponseBody} ->
						{ok, Status, ResponseHeaders, jiffy:decode(ResponseBody, [return_maps])};
				Other ->
						Other
		end.


baseurl() ->
	"http://127.0.0.1:8080".
