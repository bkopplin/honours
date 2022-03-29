-module(eneo_http).

-export([parse_body/1, read_entire_body/1, reply/3, error/4]).


%% @doc Reads the entire http body of a cowboy request object and returns the
%% body as an erlang map. Throws {invalid_json, Req} if the http body is invalid
%% json.
-spec parse_body(Req0 :: cowboy:req()) -> {ok, map(), cowboy:req()}.
parse_body(Req0) ->
	{Body, Req} = read_entire_body(Req0),
	try jiffy:decode(Body, [return_maps]) of
		Body1 -> {ok, Body1, Req}
	catch
		_:_:_ -> throw({invalid_json, Req})
	end.

%% @doc takes a cowboy:req() object and returns the entire body of that object
-spec read_entire_body(Req0 :: cowboy:req()) -> {binary(), cowboy:req()}.
read_entire_body(Req0) ->
	case cowboy_req:read_body(Req0) of
		{ok, Data, Req1} -> {Data, Req1};
		{more, Data, Req1} -> 
					{B, Req2} = read_entire_body(Req1),
					{<<Data/binary, B/binary>>, Req2}
	end.

%% @doc Sends a reply back to the client.
%% Takes in a Map as Data, so that an arbitrary format can be returned, e.g. plain html, json, xml. Currently, only json is supported
-spec reply(pos_integer(), map() | [any()], cowboy_req:req()) -> cowboy_req:req().
reply(ResponseCode, Data, Req) ->
	cowboy_req:reply(ResponseCode, #{<<"content-type">> => <<"application/json">>},
			 jiffy:encode(Data), Req).

%% @doc Sends an error back to the client. 
%% Note that Errcode is specified as lowercase atom and should correspond to matrix 
%% common error code (see @url(https://spec.matrix.org/latest/client-server-api/#common-error-codes). 
-spec error(pos_integer(), atom(), binary(), cowboy_req:req()) -> cowboy_req:req().
error(ResCode, Errcode, Error, Req) ->
	Ec = erlang:list_to_binary(string:to_upper(erlang:atom_to_list(Errcode))),	
	reply(ResCode, #{<<"errcode">> => Ec, <<"error">> => Error}, Req).

