-module(eneo_lib).

-export([gen_access_token/0, gen_device_id/0, gen_state_id/0, authenticate_token/1]).

gen_access_token() ->
	base64:encode(crypto:strong_rand_bytes(30)).

gen_device_id() ->
	base64:encode(crypto:strong_rand_bytes(20)).

gen_state_id() ->
	Id = base64:encode(crypto:strong_rand_bytes(20)),
	<<"$",Id/binary >>.

%% @doc Checks if a given user token is valid and returns the corresponding user_id or error
%% code, as the case may be.
-spec authenticate_token(cowboy:req()) -> {ok, binary()} | {error, integer(), atom(), binary()}.
authenticate_token(Req) ->
	case parse_token(Req) of
		{error, Status, Type, Error} ->
			{error, Status, Type, Error};
		{ok, Token} ->
			case db:validate_token(Token) of
				{ok, UserId} ->
					{ok, UserId};
				{error, invalid_token} ->
					{error, 401, m_unknown_token, <<"Invalid token passed.">>}
			end
	end.

parse_token(Req) ->
	try 
		cowboy_req:parse_header(<<"authorization">>, Req, undefined) 
	of
		undefined ->
			case cowboy_req:match_qs([{access_token, nonempty, undefined}], Req) of
				#{access_token := undefined} -> 
					{error, 401, m_unknown_token, <<"Missing access token.">>};
				#{access_token := Tk1} ->
					{ok, Tk1}
			end;
		{bearer, <<_:0/binary>>} ->
			{error, 401, m_missing_token, <<"Missing access token.">>};
		{bearer, Tk2} ->
			{ok, Tk2}
	catch
		exit:{request_error, _,_}:_ ->
			{error, 401, m_missing_token, <<"Invalid Authorization header.">>} %TODO check status code
	end.
