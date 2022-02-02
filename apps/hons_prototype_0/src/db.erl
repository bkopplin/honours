-module(db).

-export([get_event/2]).
-export([connect/0]).

-include_lib("epgsql/include/epgsql.hrl").

%% Connects to the local database
%% Connection information is hardcoded for the time being
connect() ->
	{ok, C} = epgsql:connect(#{
	    host => "localhost",
	    username => "bjarne",
	    password => "password",
	    database => "bjarne",
	    timeout => 4000
	}), C.

%% Gets a single event with identification EventId in room with identification RoomId.
get_event(RoomId, EventId) ->
	C = connect(),
	case epgsql:equery(C, "SELECT * FROM Events WHERE room_id = $1 AND event_id = $2;", [RoomId, EventId]) of
		{ok, Cols, [Row|_]} -> 
			ok = epgsql:close(C),
			RowList = erlang:tuple_to_list(Row),
			{ok, maps:from_list(convert_to_keypair(Cols, RowList))};
		{ok, _, []} ->
			ok = epgsql:close(C),
			{error, not_found};
		{error, Reason} ->
			ok = epgsql:close(C),
			{error, Reason}
	end.

%% Converts a list of column tuples and a list of row contents to a key-value map that maps a column name to a row cell
convert_to_keypair([Col|RemCols], [RowElement|RemRow]) ->
	ColTitle = Col#column.name,
	case Col#column.type of
		jsonb ->
			[{ColTitle, jiffy:decode(RowElement, [return_maps])}|convert_to_keypair(RemCols, RemRow)];
		_ ->
			[{ColTitle, RowElement}|convert_to_keypair(RemCols, RemRow)]
	end;
convert_to_keypair([], []) ->
	[].

