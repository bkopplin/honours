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
			%{"/rooms/:roomId/send/:eventType/:txnid", rooms, [send_message]},
			{"/rooms/:roomId/send/:eventType/:txnid", eneo_rooms_send_h, []},

			%% user authentication
			{"/account/whoami", eneo_whoami_h, []},
			%{"/login", eneo_auth, [login]}
			{"/login", eneo_login_h, []}
	      	]}
	]),
	{ok, _} = cowboy:start_clear(http, [
					    {port, 8080},
						{send_timeout, 30000},
						{keepalive, true}
					   ], #{
					       env => #{dispatch => Dispatch},
						   middlewares => [cowboy_router, logger_mw, cowboy_handler]
					      }),
    eneo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
