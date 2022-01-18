%%%-------------------------------------------------------------------
%% @doc hons_prototype_0 public API
%% @end
%%%-------------------------------------------------------------------

-module(hons_prototype_0_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
		       %% {route, erlang module, [handle, options ... ]}
		       	%% account management
			{"/whoami", matrix_auth, [whoami]},
			{"/login", matrix_auth, [login]},
			{"/_matrix/client/r0/login", matrix_auth, [login]},
			{"/_matrix/client/v3/account/whoami", matrix_auth, [whoami]},
			%% room participation
			{"/sync", rooms, [sync]},
			{"/rooms/:roomId/messages", rooms, [messages]},
			{"/_", matrix_auth, [logging]}
	      	]}
	]),
	{ok, _} = cowboy:start_clear(http, [
					    {port, 8080}
					   ], #{
					       env => #{dispatch => Dispatch}
					      }),
    hons_prototype_0_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
