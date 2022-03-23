-module(eneo_auth).

-export([init/2]).

-define(HOSTNAME, <<"localhost">>).

-ifdef(TEST).
-compile(export_all).
-endif.

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	[Action|_] = Opts,
	Req1 = handle(Method, Action, Req),
	{ok, Req1, Opts}.

handle(<<"GET">>, whoami, Req) ->
	done;

handle(<<"GET">>, login, Req) ->
	SupportedLogins = #{<<"flows">> => [
										 #{<<"type">> => <<"m.login.password">>}
										]
						},
	eneo_http:reply(200, SupportedLogins, Req);

handle(<<"POST">>, login, Req0) ->
	SupportedLoginTypes = [<<"m.login.password">>],
	SupportedIdTypes = [<<"m.id.user">>],
	try
		{ok, Body, Req1} = eneo_http:parse_body(Req0),
		Identifier = maps:get(<<"identifier">>, Body),
		LoginTypeTmp = maps:get(<<"type">>, Body),
		LoginType = case lists:member(LoginTypeTmp, SupportedLoginTypes) of
						true ->
							LoginTypeTmp;
						false ->
							throw({m_unknown,
								   <<"Unknown login type m.login.unknown">>
								  })
					end,
		IdentifierTypeTmp = maps:get(<<"type">>, Identifier),
		IdentifierType = case lists:member(IdentifierTypeTmp, SupportedIdTypes) of
							true ->
								IdentifierTypeTmp;
							false ->
								throw({m_unknown,
									   <<"Unknown login identifier type">>
									  })
						 end,
		User = maps:get(<<"user">>, Identifier),
		UserId = <<"@",User/binary,":",?HOSTNAME/binary>>,
		Password = maps:get(<<"password">>, Body),
		ok
	of
		ok ->
			case authenticate(UserId, Password) of
				true ->
					AuthObj = #{
					  <<"user_id">> => UserId,
					  <<"access_token">> => gen_access_token(),
					  <<"home_server">> => ?HOSTNAME,
					  <<"device_id">> => gen_device_id(),
					  <<"well_known">> => #{
						  <<"m.homeserver">> => #{
						  <<"base_url">> => <<"https://localhost">>
						 }
						 }
					 },
					eneo_http:reply(200, AuthObj, Req1);
				false ->
					eneo_http:error(403,  <<"M_FORBIDDEN">>, <<"Invalid password">>, Req1)
			end
	catch
		throw:{invalid_json, Req5}:_ ->
			eneo_http:error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req5);
		throw:{m_unknown, Msg}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, Msg, Req0);
		error:{badkey, <<"identifier">>}:_ ->
			eneo_http:error(400, <<"M_INVALID_PARAM">>, <<"Invalid login submission">>, Req0);
		error:{badkey, <<"type">>}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, <<"Missing JSON keys.">>, Req0);
		error:{badkey, Key}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, <<"'", Key/binary, "' not in content">>, Req0);
		error:{badmap, _}:_ ->
			eneo_http:error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req0)
	end;


handle(_, _, Req) ->
	cowboy_req:reply(200, #{}, "not implemented",  Req).

%%% --------------------------
%%% Internal Helper Functions
%%% --------------------------
-spec authenticate(User :: binary(), Password :: binary()) -> boolean().
authenticate(User, Password) ->
	StoredHash = case db:get_password(User) of
		{ok, H} -> H;
		{error, Msg} -> false
				 end,
	Hash = base64:encode(crypto:hash(sha256, Password)),
	StoredHash =:= Hash. 

gen_access_token() ->
	base64:encode(crypto:strong_rand_bytes(30)).

gen_device_id() ->
	base64:encode(crypto:strong_rand_bytes(20)).

