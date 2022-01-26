-module(rooms).

-export([init/2]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	[Action|_] = Opts,
	Req1 = handle(Method, Action, Req),
	{ok, Req, Opts}.

handle(<<"GET">>, sync, Req) ->
	cowboy_req:reply(200, #{
	  <<"content-type">> => <<"application/json">>
	 }, "sync", Req);

%% Get a single event by event ID.
handle(<<"GET">>, event, Req) ->
	RoomId = cowboy_req:binding(roomId, Req),
	EventId = cowboy_req:binding(eventId, Req),

	case db:get_event(RoomId, EventId) of
		{ok, Event} -> reply(200, Event, Req);
		{error, not_found} -> reply(404, #{
					<<"errcode">> => <<"M_NOT_FOUND">>,
					<<"error">> => <<"Could not find event {EVENTID}">>
				       }, Req)
	end;


handle(<<"GET">>, messages, Req) ->
	cowboy_req:reply(200, #{
	  <<"content-type">> => <<"application/json">>
	 }, "accessing messages" , Req).


%% Sends a reply back to the client.
%% Takes in a Map as Data, so that an arbitrary format can be returned, e.g. plain html, json, xml. Currently, only json is supported
reply(ResponseCode, Data, Req) ->
	cowboy_req:reply(ResponseCode, #{<<"content-type">> => <<"application/json">>},
			 jiffy:encode(Data), Req).
