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
			{"/whoami", matrix_auth, [whoami]},
			{"/login", matrix_auth, [login]}
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
