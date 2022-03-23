-module(eneo_auth).

-export([init/2]).

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
	io:format("StoredHash: ~p~nHash: ~p~n", [StoredHash, Hash]),
	StoredHash =:= Hash. 

random_token() ->
	base64:encode(crypto:strong_rand_bytes(30)).
