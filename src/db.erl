-module(db).

-export([get_event/2, get_messages/2]).
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
			{ok,match_col_row(Cols, RowList)};
		{ok, _, []} ->
			ok = epgsql:close(C),
			{error, not_found};
		{error, Reason} ->
			ok = epgsql:close(C),
			{error, Reason}
	end.

%% Returns a list of messages in room RoomId with filter parameters Qs
get_messages(RoomId, Qs) ->
	C = connect(),
	Order = case maps:get(dir, Qs) of <<"f">> -> "ASC"; _ -> "DESC" end,
	Limit = maps:get(limit, Qs),
	case epgsql:equery(C, "SELECT content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key FROM Events WHERE room_id = $1 ORDER BY depth DESC LIMIT $2", [RoomId, Limit]) of
		{ok, Cols, Rows} ->
			ok = epgsql:close(C),
			{ok,table_to_list(Cols, Rows)};
		{ok, _, []} ->
			ok = epgsql:close(C),
			{error, not_found};
		{error, Reason} ->
			ok = epgsql:close(C),
			{error, Reason}
	end.

table_to_list(Cols, [Row|Rest]) ->
	[maps:from_list(match_col_row(Cols, erlang:tuple_to_list(Row)))|table_to_list(Cols, Rest)];
table_to_list(Cols, []) -> [].

%% Converts a list of column tuples and a list of row contents to a key-value map that maps a column name to a row cell
match_col_row([Col|RemCols], [RowElement|RemRow]) ->
	ColTitle = Col#column.name,
	case Col#column.type of
		jsonb ->
			[{ColTitle, jiffy:decode(RowElement, [return_maps])}|match_col_row(RemCols, RemRow)];
		_ ->
			[{ColTitle, RowElement}|match_col_row(RemCols, RemRow)]
	end;
match_col_row([], []) ->
	[].

