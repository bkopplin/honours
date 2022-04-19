-module(logger_mw).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(#{path := Path, method := Method} = Req, Env) ->
	io:format("[ACCESS] ~s ~s~n", [Method, Path]),
	{ok, Req, Env}.
