-module(eneo_lib).

-export([gen_access_token/0, gen_device_id/0, gen_state_id/0, authenticate_token/1]).

gen_access_token() ->
	base64:encode(crypto:strong_rand_bytes(30)).

gen_device_id() ->
	base64:encode(crypto:strong_rand_bytes(20)).

gen_state_id() ->
	Id = base64:encode(crypto:strong_rand_bytes(20)),
	<<"$",Id/binary >>.

authenticate_token(Req) ->
	case parse_token(Req) of
		{error, Type, Error} ->
			{error, Type, Error};
		{ok, Token} ->
			case db:validate_token(Token) of
				{ok, UserId} ->
					{ok, Req#{user_id => UserId}};
				{error, invalid_token} ->
					{error, invalid_token, <<"Invalid token specified">>}
			end
	end.

parse_token(Req) ->
	try 
		cowboy_req:parse_header(<<"authorization">>, Req, undefined) 
	of
		undefined ->
			case cowboy_req:match_qs([{access_token, nonempty, undefined}], Req) of
				#{access_token := Tk1} ->
					{ok, Tk1};
				undefined -> 
					{error, missing_token, <<"Missing access token">>}
			end;
		{bearer, <<_:0/binary>>} ->
			{error, missing_token, <<"Missing access token">>};
		{bearer, Tk2} ->
			{ok, Tk2}
	catch
		exit:{request_error, _,_}:_ ->
			{error, invalid_header, <<"Invalid Authorization header">>}
	end.
