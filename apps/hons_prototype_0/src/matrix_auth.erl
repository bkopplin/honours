-module(matrix_auth).

-export([init/2]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	[Action|_] = Opts,
	Req1 = handle(Method, Action, Req),
	{ok, Req, Opts}.

handle(<<"GET">>, whoami, Req) ->
	Qs = cowboy_req:parse_qs(Req),
	case lists:keyfind(<<"access_token">>, 1, Qs) of
		false -> send_error(401, <<"M_MISSING_TOKEN">>, <<"Missing access token">>, Req);
		{<<"access_token">>, T} -> Token = T,
					   io:format("qs: ~w, token: ~s~n", [Qs, Token]),
					   %% In a further release, look up the access token and return user_id and device_id
					   Reply = jiffy:encode(#{
					     <<"user_id">> => <<"@user:example.org">>,
					     <<"device_id">> => <<"AMLCDHVEEX">>
					    }),
					   cowboy_req:reply(200, #{
					     <<"content-type">> => <<"application/json">>
					    }, Reply, Req)

	end;

handle(<<"GET">>, login, Req) ->
	SupportedLogins = [
			   <<"{'flows': [">>,
			   <<"{'type': 'm.login.password'}">>,
			   <<"]}">>
			  ],
	cowboy_req:reply(200, #{
	  <<"content-type">> => <<"application/json">>
	 }, SupportedLogins, Req);

handle(<<"POST">>, login, Req1) ->
	{ok, Body, Req} = cowboy_req:read_body(Req1), %% TODO consider {more, data}
	JSON = jiffy:decode(Body, [return_maps]),

	Identifier = try maps:get(<<"identifier">>, JSON)
		     catch error:_ -> send_400(<<"M_INVALID_PARAM">>, <<"Invalid login submission">>, Req) end,

	User = try maps:get(<<"user">>, Identifier)
	       catch error:E -> send_400(<<"M_UNKNOWN">>, <<"User identifier is missing 'user' key">>, Req) end,

	Password = try maps:get(<<"password">>, JSON)
		   catch error:_ -> send_400(<<"M_INVALID_PARAM">>, <<"Bad parameter: password">>, Req) end,

	Authenticated = authenticate(User, Password),
	case Authenticated of	
		true -> true;
		false -> send_error(403, <<"M_FORBIDDEN">>, <<"Invalid password">>, Req)
	end,

	AuthObj = #{
	  <<"user_id">> => iolist_to_binary([<<"@">>, User, <<":localhost">>]),
	  <<"access_token">> => <<"syt_ZGV2Yms_vEGGntRjZePskAtUiUaF_1mohKk">>,
	  <<"home_server">> => <<"localhost">>,
	  <<"device_id">> => <<"YVYMCHYZDP">>,
	  <<"well_known">> => #{
	      <<"m.homeserver">> => #{
		  <<"base_url">> => <<"https://matrix-client.localhost">>
		 }
	     }
	 },
	ReplyJSON = jiffy:encode(AuthObj),
	cowboy_req:reply(200, #{
	  <<"content-type">> => <<"text/json">>
	 }, ReplyJSON, Req);

handle(<<"GET">>, logging, Req) ->
	io:format("user connected to ~s~n", [cowboy_req:path()]),
	cowboy_req:reply(200, Req);

handle(_, _, Req) ->
	cowboy_req:reply(200, #{}, "not implemented",  Req).

send_error(HttpErrCode, Errcode, Error, Req) ->
	Msg = [<<"{\"errcode\": \"">>, Errcode, <<"\", \"error\": \"">>, Error, <<"\"}">>],
	cowboy_req:reply(HttpErrCode, #{<<"content-type">> => <<"application/json">>}, Msg, Req),
	exit(Error).

send_400(Errcode, Error, Req) ->
	Msg = [<<"{\"errcode\": \"">>, Errcode, <<"\", \"error\": \"">>, Error, <<"\"}">>],
	cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Msg, Req),
	exit(Error).

authenticate(User, Password) ->
	PasswordDb = #{
	  <<"Aragon">> => <<"password">>,
	  <<"Bilbo">> => <<"ilikereading">>
	 },
	case maps:is_key(User, PasswordDb) of
		true ->
			#{User := P} = PasswordDb,
			Password =:= P;
		false ->
			false
	end.
