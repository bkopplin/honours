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
		{ok, []} -> reply(404, #{
					<<"errcode">> => <<"M_NOT_FOUND">>,
					<<"error">> => <<"Could not find event {EVENTID}">>
				       }, Req);
		{ok, [Event|_]} -> reply(200, Event, Req);
		{error, _Reason} -> reply(404, #{
				<<"errcode">> => <<"UNIMPLEMENTED">>,
				<<"error">> => <<"unimplemented">>}, Req)
	end;


handle_request(<<"GET">>, messages, Req) ->
	RoomId = cowboy_req:binding(roomId, Req),
	Qs = cowboy_req:match_qs([{limit, int, 10}, {dir, [], <<"b">>}, {from, [], start}], Req),
	io:format("QS: ~p~n", [Qs]),
	case db:get_messages(RoomId, Qs) of
		{ok, Events} -> reply(200, Events, Req);
		{error, _} -> reply(404, #{<<"error">> => <<"error while querying the database, no events returned">>}, Req)
	end;

handle_request(<<"PUT">>, send_message, Req0) ->
	try
		RoomId = cowboy_req:binding(roomId, Req0),
		TxnId = cowboy_req:binding(txnId, Req0),
		Sender = <<"@clyde:localhost">>,
		{ok, Body, Req} = parse_body(Req0),
		Message = maps:get(<<"body">>, Body),
		MsgType = maps:get(<<"msgtype">>, Body),
		ok
	of
		ok ->
			case db:send_message(Message, RoomId, Sender, TxnId) of
				{ok, EventId} -> 
					reply(200, #{<<"event_id">> => EventId}, Req);
				{error, unknown_room} -> 
					reply_error(403, <<"M_FORBIDDEN">>, <<"Unknown room">>, Req)
			end 
	catch
		throw:{invalid_json, Req1}:_ ->
			reply_error(400, <<"M_NOT_JSON">>, <<"Content not JSON.">>, Req1);
		error:{badkey, Key}:_ ->
			reply_error(400, <<"M_UNKNOWN">>, <<"'", Key/binary, "' not in content">>, Req0)
	end.


%% --------------------
%% Helper functions
%% --------------------

parse_body(Req0) ->
	{Body, Req} = read_entire_body(Req0),
	try jiffy:decode(Body, [return_maps]) of
		Body1 -> {ok, Body1, Req}
	catch
		_:_:_ -> throw({invalid_json, Req})
	end.

read_entire_body(Req0) ->
	case cowboy_req:read_body(Req0) of
		{ok, Data, Req1} -> {Data, Req1};
		{more, Data, Req1} -> 
					{B, Req2} = read_entire_body(Req1),
					{<<Data/binary, B/binary>>, Req2}
	end.

%% @doc Sends a reply back to the client.
%% Takes in a Map as Data, so that an arbitrary format can be returned, e.g. plain html, json, xml. Currently, only json is supported
-spec reply(pos_integer(), map() | [any()], cowboy_req:req()) -> cowboy_req:req().
reply(ResponseCode, Data, Req) ->
	cowboy_req:reply(ResponseCode, #{<<"content-type">> => <<"application/json">>},
			 jiffy:encode(Data), Req).

reply_error(ResCode, Errcode, Error, Req) ->
	reply(ResCode, #{<<"errcode">> => Errcode, <<"error">> => Error}, Req).
