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
		  % eunit tests
		   ?setup(fun eunit1/1),
		   ?setup(fun eunit2/1),
		   ?setup(fun eunit3/1),
		   
		   % send messages
		   ?setup(fun t_send_message_correct/1),
		   ?setup(fun t_send_message_nonexisting_room/1),
		   ?setup(fun t_send_message_missing_body/1),
		   ?setup(fun t_send_message_content_field_missing/1),

		   % log in
		   ?setup(fun t_login_successful/1),
		   ?setup(fun t_login_missing_type_key/1),
		   ?setup(fun t_login_unknown_login_type/1),
		   ?setup(fun t_login_unkown_identifier_type/1),
		   ?setup(fun t_login_user_not_provided/1),
		   ?setup(fun t_login_invalid_password/1),

		   % supported login types
		   ?setup(fun t_successful_supported_login/1),

		   % whoami
		   ?setup(fun t_whoami_successful/1),
		   ?setup(fun t_whoami_missing_token/1),
		   ?setup(fun t_whoami_invalid_token/1)
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

t_send_message_correct(C) ->
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

t_send_message_nonexisting_room(_C) ->
	ReqBody =  #{
       <<"body">> => <<"bar">>,
       <<"msgtype">> => <<"m.text">>
	},
	{"send a message to a nonexisting room", 
	 ?_assertMatch(
	    {ok, "403", _, #{<<"errcode">> := <<"M_FORBIDDEN">>, <<"error">> := <<"Unknown room">> }},
		send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", [<<"!invalid:room">>, "1", ?TOKEN1], put, ReqBody)
	  )}.

t_send_message_missing_body(_C) ->
	{ok, Status, _, Body} = send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", [?ROOM1, "1", ?TOKEN1], put, []),
	[
	 ?_assertMatch("400", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_NOT_JSON">>, <<"error">> := <<"Content not JSON.">>}, Body)
	].

t_send_message_content_field_missing(_C) ->
	ReqBody =  #{
       <<"body">> => <<"bar">>
	},
	?_assertMatch(
		{ok, "400", _, #{<<"errcode">> := <<"M_UNKNOWN">>}},
		send_http("/rooms/~s/send/m.room.message/~saccess_token=~s", [?ROOM1, "1", ?TOKEN1], put, ReqBody)).

%%% ------------
%%% Log in
%%% ------------


t_login_successful(C) ->
	db:insert_user(C, "@neo:localhost", "thematrix"),
	ReqBody = #{
	  <<"type">> => <<"m.login.password">>,
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.user">>,
		  <<"user">> => <<"neo">>
		 },
	  <<"password">> => <<"thematrix">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, Status, _, ResBody} = send_http("/login", [], post, ReqBody),
	[
	 ?_assertEqual("200", Status),
	 ?_assertEqual(<<"@neo:localhost">>, maps:get(<<"user_id">>, ResBody, "undefined")),
	 ?_assertEqual(<<"localhost">>, maps:get(<<"home_server">>, ResBody, "undefined")),
	 ?_assert(maps:is_key(<<"access_token">>, ResBody))
	].
t_login_missing_type_key(_C) ->
	ReqBody = #{
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.user">>,
		  <<"user">> => <<"neo">>
		 },
	  <<"password">> => <<"thematrix">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, Status, _, ResBody} = send_http("/login", [], post, ReqBody),
	[
	 ?_assertEqual("400", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_UNKNOWN">>, <<"error">> := <<"Missing JSON keys.">>}, ResBody)
	].
t_login_unknown_login_type(_C) ->
	ReqBody = #{
	  <<"type">> => <<"m.login.unknown">>,
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.user">>,
		  <<"user">> => <<"neo">>
		 },
	  <<"password">> => <<"thematrix">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, Status, _, ResBody} = send_http("/login", [], post, ReqBody),
	[
	 ?_assertEqual("400", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_UNKNOWN">>, <<"error">> := <<"Unknown login type m.login.unknown">>}, ResBody)
	].

t_login_unkown_identifier_type(_C) ->
	ReqBody = #{
	  <<"type">> => <<"m.login.password">>,
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.unknown">>,
		  <<"user">> => <<"neo">>
		 },
	  <<"password">> => <<"thematrix">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, Status, _, ResBody} = send_http("/login", [], post, ReqBody),
	[
	 ?_assertEqual("400", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_UNKNOWN">>, <<"error">> := <<"Unknown login identifier type">>}, ResBody)
	].

t_login_user_not_provided(_C) ->
	ReqBody = #{
	  <<"type">> => <<"m.login.password">>,
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.user">>
		 },
	  <<"password">> => <<"thematrix">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, Status, _, ResBody} = send_http("/login", [], post, ReqBody),
	[
	 ?_assertEqual("400", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_UNKNOWN">> }, ResBody)
	].

t_login_invalid_password(_C) ->
	ReqBody = #{
	  <<"type">> => <<"m.login.password">>,
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.user">>,
		  <<"user">> => <<"neo">>
		 },
	  <<"password">> => <<"wrong">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, Status, _, ResBody} = send_http("/login", [], post, ReqBody),
	[
	 ?_assertEqual("403", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_FORBIDDEN">>, <<"error">> := <<"Invalid password">>}, ResBody)
	].

%%% -----------------------
%%% Supported Login Types
%%% -----------------------
t_successful_supported_login(_C) ->
	{ok, Status, _, ReplyBody} = send_http("/login", [], get),
	[
	 ?_assertEqual("200", Status),
	 ?_assertEqual(#{<<"flows">> => [#{<<"type">> => <<"m.login.password">>}]}, ReplyBody)
	].
%%% ---------------
%%% Whoami
%%% --------------

t_whoami_successful(C) ->
	db:insert_user(C, "@neo:localhost", "thematrix"),
	ReqBodyLogin = #{
	  <<"type">> => <<"m.login.password">>,
	  <<"identifier">> => #{
		  <<"type">> => <<"m.id.user">>,
		  <<"user">> => <<"neo">>
		 },
	  <<"password">> => <<"thematrix">>,
	  <<"initial_device_display_name">> => <<"Nebuchadnezzar">>
	 },
	{ok, "200", _, ResBodyLogin} = send_http("/login", [], post, ReqBodyLogin),
	Token = maps:get(<<"access_token">>, ResBodyLogin),
	{ok, Status, _, Body} = send_http(<<"/whoami?access_token=",Token/binary>>, [], get, []), 
	[
	 ?_assertEqual("200", Status),
	 ?_assertMatch(#{<<"user_id">> := <<"@neo:localhost">>, <<"is_guest">> := false}, Body)
	].
t_whoami_missing_token(_C) ->
	{ok, Status, _, Body} = send_http("/whoami", [], get, []), 
	[
	 ?_assertEqual("401", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_MISSING_TOKEN">>, <<"error">> := <<"Missing access token">>}, Body)
	].
t_whoami_invalid_token(_C) ->
	{ok, Status, _, Body} = send_http("/whoami?access_token=a", [], get, []), 
	[
	 ?_assertEqual("401", Status),
	 ?_assertMatch(#{<<"errcode">> := <<"M_UNKNOWN_TOKEN">>, <<"error">> := <<"Invalid macaroon passed">>}, Body)
	].
%%% ------------------------
%%% Internal Helper Functions
%%% -------------------------

send_get(Url, Args) -> send_http(Url, Args, get, []).
send_post(Url, Args) -> send_http(Url, Args, post, []).
send_put(Url, Args) -> send_http(Url, Args, put, []).

send_http(Url, Args, Method) ->
	send_http(Url, Args, Method, []).

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

