%% @doc Provides enpoints for room participation.
%% HTTP requests will be routed to {@link init/2. init/2} with cowboy
%% @end

-module(rooms).

-export([init/2]).

-type http_action() :: event | messages.

%% @doc Serves as an entry point for the cowboy router.
%% Takes in a request object and a list of options where the first element is
%% the action to perform. Actions correspond to matrix endpoints. See the matrix documentation
%% and the http_action() type specification for more information on which actions match which 
%% matrix endpoint. 
%% For instance, the Action atom "messages" corresponds to the matrix endpoint at /rooms/{room_id}/messages.
%% @end

-spec init(Req :: cowboy_req:req(), Opts :: [atom()]) -> {ok, cowboy:req(), [atom()]}.

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	[Action|_] = Opts,
	_Req1 = handle_request(Method, Action, Req),
	{ok, Req, Opts}.


%% @doc handles an API call.
%% Takes in a http method, an action and a request object, performs the necessary actions
%% and sends back a http response.

-spec handle_request(Method::binary(), Action::http_action(), Req::cowboy_req:req()) -> cowboy_req:req(). 

handle_request(<<"GET">>, event, Req) ->
	RoomId = cowboy_req:binding(roomId, Req),
	EventId = cowboy_req:binding(eventId, Req),

	case db:get_event(RoomId, EventId) of
		{ok, [Event|_]} -> eneo_http:reply(200, Event, Req);
		{ok, []} -> eneo_http:reply(404, #{
					<<"errcode">> => <<"M_NOT_FOUND">>,
					<<"error">> => <<"Could not find event ", EventId/binary>>
				       }, Req);
		{error, _Reason} -> eneo_http:reply(404, #{
				<<"errcode">> => <<"UNIMPLEMENTED">>,
				<<"error">> => <<"unimplemented">>}, Req)
	end;


handle_request(<<"GET">>, messages, Req) ->
	RoomId = cowboy_req:binding(roomId, Req),
	Qs = cowboy_req:match_qs([{limit, int, 10}, {dir, [], <<"b">>}, {from, [], start}], Req),
	io:format("QS: ~p~n", [Qs]),
	case db:get_messages(RoomId, Qs) of
		{ok, Events} -> eneo_http:reply(200, Events, Req);
		{error, _} -> eneo_http:reply(404, #{<<"error">> => <<"error while querying the database, no events returned">>}, Req)
	end;

handle_request(<<"PUT">>, send_message, Req0) ->
	try
		RoomId = cowboy_req:binding(roomId, Req0),
		TxnId = cowboy_req:binding(txnId, Req0),
		Sender = <<"@clyde:localhost">>,
		{ok, Body, Req} = eneo_http:parse_body(Req0),
		Message = maps:get(<<"body">>, Body),
		_MsgType = maps:get(<<"msgtype">>, Body),
		ok
	of
		ok ->
			case db:send_message(Message, RoomId, Sender, TxnId) of
				{ok, EventId} -> 
					eneo_http:reply(200, #{<<"event_id">> => EventId}, Req);
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


%% --------------------
%% Helper functions
%% --------------------
