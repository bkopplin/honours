%%
%% Middleware to log output to file and to stdout.
%% Further customization is required in the future. It would also be desirable to run the logger as a event handler in a separate process.
-module(logger_mw).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(#{path := Path, method := Method} = Req, Env) ->
	file:write_file("/var/log/eneo.log", <<"[ACCESS] ", Method/binary, " ", Path/binary, "\n">>, [append]),
	io:format("[ACCESS] ~s ~s~n", [Method, Path]),
	{ok, Req, Env}.
