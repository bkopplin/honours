%%%-------------------------------------------------------------------
%% @doc eneo public API
%% @end
%%%-------------------------------------------------------------------

-module(eneo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
		       %% {route, erlang module, [handle, options ... ]}
		       	%% account management
			{"/whoami", eneo_auth, [whoami]},
			%% room participation
			{"/rooms/:roomId/messages", rooms, [messages]},
			{"/rooms/:roomId/event/:eventId", rooms, [event]},
			{"/rooms/:roomId/send/:eventType/:txnid", rooms, [send_message]},

			%% user authentication
			{"/account/whoami", eneo_whoami_h, []},
			%{"/login", eneo_auth, [login]}
			{"/login", eneo_login_h, []}
	      	]}
	]),
	{ok, _} = cowboy:start_clear(http, [
					    {port, 8080}
					   ], #{
					       env => #{dispatch => Dispatch}
					      }),
    eneo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
