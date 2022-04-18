-module(eneo_rooms_send_h).

-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

-define(HOSTNAME, <<"localhost">>).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, _State) ->
    {[<<"PUT">>], Req, #{}}.

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
	  {{<<"application">>, <<"json">>, '*'}, to_json}
	 ], Req, State}.

content_types_accepted(Req, State) ->
	{[
	  {{<<"application">>, <<"json">>, '*'}, to_json}
	 ], Req, State}.

to_json(Req0, #{user_id := UserId} = State) ->
	try
		RoomId = cowboy_req:binding(roomId, Req0),
		TxnId = cowboy_req:binding(txnId, Req0),
		Sender = UserId,
		{ok, Body, Req} = eneo_http:parse_body(Req0),
		Message = maps:get(<<"body">>, Body),
		_MsgType = maps:get(<<"msgtype">>, Body),
		ok
	of
		ok ->
			case db:send_message(Message, RoomId, Sender, TxnId) of
				{ok, EventId} -> 
					ResBody = jiffy:encode(#{<<"event_id">> => EventId}),
					Req1 = cowboy_req:set_resp_body(ResBody, Req),
					{true, Req1, State};
				{error, unknown_room} -> 
					eneo_http:error(403, <<"M_FORBIDDEN">>, <<"Unknown room">>, Req)
			end 
	catch
		throw:{invalid_json, Req1}:_ ->
			eneo_http:error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req1);
		error:{badkey, Key}:_ ->
			eneo_http:error(400, <<"M_UNKNOWN">>, <<"'", Key/binary, "' not in content">>, Req0);
		error:{badmap, _}:_ ->
			eneo_http:error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req0)

	end.
