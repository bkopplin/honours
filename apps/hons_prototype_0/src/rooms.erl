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

handle(<<"GET">>, messages, Req) ->
	% access bindings: roomId
	io:format("[DEBUG] roomId: ~s~n", [cowboy_req:binding(roomId, Req)]),

	% access query string: access token
	ParsedQs = cowboy_req:parse_qs(Req),

	cowboy_req:reply(200, #{
	  <<"content-type">> => <<"application/json">>
	 }, "accessing messages" , Req).
