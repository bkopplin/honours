%% @doc The module provides an API for database queries.
%% For the time being, this module has no support for concurrency.
%% At a later stage, it is expected to make use of OTP patterns.
%% @end

-module(db).

-export([get_event/2, get_messages/2]).
-export([connect/0]).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("eunit/include/eunit.hrl").

-type qs() :: #{limit => pos_integer(),
		dir => binary(),
		from => start}.
-type room_id() :: string() | binary().
-type event_id() :: string() | binary().



%% @doc Connects to a local hardcoded database on the developers machine.
%% This function will either be removed in a future releae or the parameters will
%% be loaded from file.
%% @end
-spec connect() -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
connect() ->
	connect("localhost", "bjarne", "password", "bjarne").

%% @doc A wrapper to the epgsql:connect/1 function.
%% The arguments to this function will be used to connect to a database.

-spec connect(Host :: string() | binary(),
	      Username :: string() | binary(),
	      Password :: string() | binary(),
	      Databse :: string() | binary()) -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.

connect(Host, Username, Password, Database) ->
	epgsql:connect(#{
	    host => Host,
	    username => Username,
	    password => Password,
	    database => Database,
	    timeout => 4000
	}).

%% @doc Gets a single event.
%% EventId and RoomId refer to the event's identification and room.
%% If no event is found then an error is returned. If more than one event
%% matches the arguments, only the first event in the list is returned.

-spec get_event(RoomId :: room_id(), EventId :: event_id()) -> {ok, any()} | {error, not_found} | epgsql_sock:error().

get_event(RoomId, EventId) ->
	{ok, C} = connect(),
	case epgsql:equery(C, "SELECT * FROM Events WHERE room_id = $1 AND event_id = $2;", [RoomId, EventId]) of
		{ok, Cols, [Row|_]} -> 
			ok = epgsql:close(C),
			RowList = erlang:tuple_to_list(Row),
			{ok,zip_col_row(Cols, RowList)};
		{ok, _, []} ->
			ok = epgsql:close(C),
			{error, not_found};
		{error, Reason} ->
			ok = epgsql:close(C),
			{error, Reason}
	end.

%% @doc Get a list of messages from a room.
%% RoomId is the id of the room to get the messages from.
%% Qs is the Query string 
%% @end

%% @TODO change {ok, any()} to reflect spec of table list
-spec get_messages(RoomId :: room_id(), Qs :: qs()) -> {ok, any()} | {error, not_found} | epgsql_sock:error().

get_messages(RoomId, Qs) ->
	{ok, C} = connect(),
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

%% @doc Converts a table into a list of maps. Given a column and a corresponding list of row tuples
%% as returned by {@link epgsql:squery/2. epgsql:squery/2}, this function creates a list of maps for every row
%% where the key is the name of a column and the value the corresponding row element. The output of this
%% function is intended to be parsed to json.
%% @end

-spec table_to_list(Cols :: [#column{}], [] | [tuple()]) -> [#{binary() => any()}].

table_to_list(Cols, [Row|Rest]) ->
	[maps:from_list(zip_col_row(Cols, erlang:tuple_to_list(Row)))|table_to_list(Cols, Rest)];
table_to_list(_Cols, []) -> [].

%% @doc Zips a list of columns and a row. The returned list is of the form [{columnName, RowElement}].
%% Note that both the column and row have to be of type list.
%% @end

-spec zip_col_row(Cols :: [#column{}] | [], Row :: tuple() | [any()] | []) -> [{binary(), any()}].

zip_col_row(Cols, Row) when is_tuple(Row) ->
	zip_col_row(Cols, erlang:tuple_to_list(Row));
zip_col_row([Col|RemCols], [RowElement|RemRow]) ->
	ColTitle = Col#column.name,
	case Col#column.type of
		jsonb when is_atom(RowElement) ->
			[{ColTitle, jiffy:decode(erlang:atom_to_binary(RowElement), [return_maps])}|zip_col_row(RemCols, RemRow)];
		jsonb ->
			[{ColTitle, jiffy:decode(RowElement, [return_maps])}|zip_col_row(RemCols, RemRow)];

		_ ->
			[{ColTitle, RowElement}|zip_col_row(RemCols, RemRow)]
	end;
zip_col_row([], []) ->
	[].

-ifdef(EUNIT).
%%% -----
%%% Tests
%%% -----
zip_col_row_empty_values_test_() ->
	[?_assertEqual([], zip_col_row([],[])),
	 ?_assertError(function_clause, zip_col_row([],[foo])),
	 ?_assertError(function_clause, zip_col_row([generate_column(<<"foo">>, text)], []))
	].
		      

zip_col_row_single_column_test_() ->
	[?_assertEqual([{<<"event_id">>, <<"123">>}], zip_col_row([generate_column(<<"event_id">>, text)], [<<"123">>])),
	?_assertEqual([{<<"event_id">>, <<"123">>}], zip_col_row([generate_column(<<"event_id">>, text)], {<<"123">>}))
	].

zip_col_row_multiple_columns_test() ->
	?assertEqual(
	   [{<<"content">>, #{<<"body">> => <<"hello world">>, <<"msgtype">> => <<"m.text">>}},
	    {<<"event_id">>, <<"123">>},
	    {<<"origin_server_ts">>, 1638547064954}], 
	   zip_col_row(
	     [generate_column(<<"content">>, jsonb),
	      generate_column(<<"event_id">>, text),
	      generate_column(<<"origin_server_ts">>, int8)
	     ], 
	     [
	 	<<"{\"body\": \"hello world\", \"msgtype\": \"m.text\"}">>, <<"123">>,1638547064954
	     ])).

db_test_() ->
	  {setup,spawn,
	   fun () ->  {ok, C} = connect("localhost", "bjarne", "password", "bjarne"), C end,
	   fun (C) -> epgsql:close(C) end,
	   fun (C) -> [
		     	?_assertMatch({ok,_, [{<<"eid4">>}]} , epgsql:equery(C, "select event_id from Events WHERE event_id = 'eid4';")) 
		      ] end
	  }.

%% -------------------------
%% Test Helper Functions
%% -------------------------

%% @doc Generates an epgsql column for testing purposes.
%% Takes in the columns name and type and sets the remaining values to default values.
%% @end

-spec generate_column(Name :: binary(), Type :: atom()) -> #column{].

generate_column(Name, Type) ->
	#column{name = Name,type = Type,oid = 3802,
        size = -1,modifier = -1,format = 0,table_oid = 16619,
        table_attr_number = 1}.
-endif.
