-module(db).

-export([get_event/2, get_messages/2]).
-export([connect/0]).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("eunit/include/eunit.hrl").

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
	Limit = maps:get(limit, Qs),
	case epgsql:equery(C, "SELECT content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key FROM Events WHERE room_id = $1 ORDER BY depth DESC LIMIT $2", [RoomId, Limit]) of
		{ok, _Cols, []} ->
			ok = epgsql:close(C),
			{error, not_found};
		{ok, Cols, Rows} ->
			ok = epgsql:close(C),
			{ok,table_to_list(Cols, Rows)};
		{error, Reason} ->
			ok = epgsql:close(C),
			{error, Reason}
	end.

table_to_list(Cols, [Row|Rest]) ->
	[maps:from_list(match_col_row(Cols, erlang:tuple_to_list(Row)))|table_to_list(Cols, Rest)];
table_to_list(_Cols, []) -> [].

%% 
%% Converts the given list of epgsql-columns and a corresponding list of row elements into a list of the form [{columnName, RowElement}].
%% Note that both the column and row have to be of type list.
%%
match_col_row([Col|RemCols], [RowElement|RemRow]) ->
	ColTitle = Col#column.name,
	case Col#column.type of
		jsonb ->
			[{ColTitle, jiffy:decode(RowElement, [return_maps])}|match_col_row(RemCols, RemRow)];
		int8 ->
			[{ColTitle, erlang:binary_to_integer(RowElement)}|match_col_row(RemCols, RemRow)];
		_ ->
			[{ColTitle, RowElement}|match_col_row(RemCols, RemRow)]
	end;
match_col_row([], []) ->
	[].

-ifdef(EUNIT).
%%% -----
%%% Tests
%%% -----
match_col_row_test() ->
	?assertEqual([], match_col_row([],[])).

match_col_row_single_column_test() ->
	?assertEqual([{<<"event_id">>, <<"123">>}], match_col_row([generate_column(<<"event_id">>, text)], [<<"123">>]) ).

match_col_row_two_columns_test() ->
	?assertEqual(
	   [{<<"content">>, #{<<"body">> => <<"hello world">>, <<"msgtype">> => <<"m.text">>}},
	    {<<"event_id">>, <<"123">>},
	    {<<"origin_server_ts">>, 1638547064954}], 
	   match_col_row(
	     [generate_column(<<"content">>, jsonb),
	      generate_column(<<"event_id">>, text),
	      generate_column(<<"origin_server_ts">>, int8)
	     ], 
	     [
	 	<<"{\"body\": \"hello world\", \"msgtype\": \"m.text\"}">>, <<"123">>,<<"1638547064954">>
	     ])).


%%% -------------------------
%%% Internal Helper Functions
%%% -------------------------

%%%
%%% Generates an epgsql column for testing purposes
%%% Name :: binary string
%%% type :: atom
%%%
generate_column(Name, Type) ->
	#column{name = Name,type = Type,oid = 3802,
        size = -1,modifier = -1,format = 0,table_oid = 16619,
        table_attr_number = 1}.
-endif.
