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
	end.

%% @doc Sends a reply back to the client.
%% Takes in a Map as Data, so that an arbitrary format can be returned, e.g. plain html, json, xml. Currently, only json is supported
-spec reply(pos_integer(), map() | [any()], cowboy_req:req()) -> cowboy_req:req().
reply(ResponseCode, Data, Req) ->
	cowboy_req:reply(ResponseCode, #{<<"content-type">> => <<"application/json">>},
			 jiffy:encode(Data), Req).
