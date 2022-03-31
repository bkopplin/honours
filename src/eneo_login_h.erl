-module(eneo_login_h).

-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
%-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([supportedLogins/2]).

-define(HOSTNAME, <<"localhost">>).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, #{}}.

is_authorized(Req, State) ->
	case eneo_lib:authenticate_token(Req) of
		{ok, UserId} ->
			{true, Req, State#{user_id => UserId}};
		{error, C, E, M} ->
			Req1 = eneo_http:error(C, E, M, Req),
			{stop, Req1, State}
	end.	

content_types_provided(Req, State) ->
	{[
	  {{<<"application">>, <<"json">>, '*'}, supportedLogins}
	 ], Req, State}.

content_types_accepted(Req, State) ->
	io:format("content_types_accepted: ~p~n", [Req]),
	{[
	  {{<<"application">>, <<"json">>, '*'}, to_json}
	 ], Req, State}.

supportedLogins(Req, State) ->
	io:format("supported logins~n"),
	SupportedLogins = #{<<"flows">> => [
						 #{<<"type">> => <<"m.login.password">>}
						]
						},
	{jiffy:encode(SupportedLogins), Req, State}.

to_json(Req, State) ->
	SupportedLoginTypes = [<<"m.login.password">>],
	SupportedIdTypes = [<<"m.id.user">>],
	try
		{ok, Body, Req1} = eneo_http:parse_body(Req),
		Identifier = maps:get(<<"identifier">>, Body),
		LoginTypeTmp = maps:get(<<"type">>, Body),
		_LoginType = case lists:member(LoginTypeTmp, SupportedLoginTypes) of
						true ->
							LoginTypeTmp;
						false ->
							throw({m_unknown,
								   <<"Unknown login type m.login.unknown">>
								  })
					end,
		IdentifierTypeTmp = maps:get(<<"type">>, Identifier),
		_IdentifierType = case lists:member(IdentifierTypeTmp, SupportedIdTypes) of
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
					{ok, AccessToken, DeviceId} = db:new_session(UserId, undefined),
					AuthObj = #{
					  <<"user_id">> => UserId,
					  <<"access_token">> => AccessToken,
					  <<"home_server">> => ?HOSTNAME,
					  <<"device_id">> => DeviceId,
					  <<"well_known">> => #{
						  <<"m.homeserver">> => #{
						  <<"base_url">> => <<"https://localhost">>
						 }
						 }
					 },
					Req2 = cowboy_req:set_resp_body(jiffy:encode(AuthObj), Req1),
					{true, Req2, State};
				false ->
					eneo_http:error(403,  <<"M_FORBIDDEN">>, <<"Invalid password">>, Req1)
					% TODO: What is best practice: let it crash or return {true, Req, State}
					% Idea: what if correct response does not lead to  exception error: no try clause matching 

			end
	catch
		throw:{invalid_json, Req5}:_ ->
			eneo_http:error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req5);
		throw:{m_unknown, Msg}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, Msg, Req);
		error:{badkey, <<"identifier">>}:_ ->
			eneo_http:error(400, <<"M_INVALID_PARAM">>, <<"Invalid login submission">>, Req);
		error:{badkey, <<"type">>}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, <<"Missing JSON keys.">>, Req);
		error:{badkey, Key}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, <<"'", Key/binary, "' not in content">>, Req);
		error:{badmap, _}:_ ->
			eneo_http:error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req)
	end.

%%% --------------------------
%%% Internal Helper Functions
%%% --------------------------
-spec authenticate(User :: binary(), Password :: binary()) -> boolean().
authenticate(User, Password) ->
	StoredHash = case db:get_password(User) of
		{ok, H} -> H;
		{error, _Msg} -> false
				 end,
	Hash = base64:encode(crypto:hash(sha256, Password)),
	StoredHash =:= Hash. 
