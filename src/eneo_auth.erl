-module(eneo_auth).

-export([init/2]).

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
	AuthObj = #{
	  <<"user_id">> => <<"">>,
	  <<"access_token">> => <<"syt_ZGV2Yms_vEGGntRjZePskAtUiUaF_1mohKk">>,
	  <<"home_server">> => <<"localhost">>,
	  <<"device_id">> => <<"YVYMCHYZDP">>,
	  <<"well_known">> => #{
	      <<"m.homeserver">> => #{
		  <<"base_url">> => <<"https://matrix-client.localhost">>
		 }
	     }
	 },
	eneo_http:reply(200, #{}, Req0);

handle(_, _, Req) ->
	cowboy_req:reply(200, #{}, "not implemented",  Req).

authenticate(User, Password) ->
	ok.

random_token() ->
	base64:encode(crypto:strong_rand_bytes(30)).
