-module(eneo_whoami_h).

-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, #{}}.

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

to_json(Req, #{user_id := Uid} = State) ->
	{ok, IsGuest} = db:is_guest(Uid),
	Res = #{user_id => Uid,
		   is_guest => IsGuest},
	{jiffy:encode(Res), Req, State}.
