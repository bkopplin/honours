%% @doc The module provides an API for database queries.
%% For the time being, this module has no support for concurrency.
%% At a later stage, it is expected to make use of OTP patterns.
%% @end

-module(db).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3]).
-export([get_event/2, get_messages/2, send_message/4]).

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(gen_server).

-include_lib("epgsql/include/epgsql.hrl").

-type qs() :: #{limit => pos_integer(),
		dir => binary(),
		from => start}.
-type room_id() :: string() | binary().
-type event_id() :: string() | binary().
-type table() :: [#{binary() => any()}].
-type event() :: #{binary() => any()}.


start_link(DbConfig) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, DbConfig, []).

stop() ->
	gen_server:stop(?MODULE).

%% @doc Gets a single event.
%% EventId and RoomId refer to the event's identification and room.
%% If no event is found then an error is returned. If more than one event
%% matches the arguments, only the first event in the list is returned.

-spec get_event(RoomId :: room_id(), EventId :: event_id()) -> {ok, event()} | {error, not_found} | epgsql_sock:error().

get_event(RoomId, EventId) ->
	gen_server:call(?MODULE, {get_event, RoomId, EventId}).

%% @doc Get a list of messages from a room.
%% RoomId is the id of the room to get the messages from.
%% Qs is the Query string 
%% @end

-spec get_messages(RoomId :: room_id(), Qs :: qs()) -> {ok, table()} | {error, not_found} | epgsql_sock:error().

get_messages(RoomId, Qs) ->
	gen_server:call(?MODULE, {get_messages, RoomId, Qs}).

%% @doc sends a message

-spec send_message(Message :: binary(), RoomId :: binary(), Sender :: binary(), TxdId :: binary()) -> {ok, binary()} | {error, unknown_room}.
send_message(Message, RoomId, Sender, TxdId) ->
	gen_server:call(?MODULE, {send_message, Message, RoomId, Sender, TxdId}).
%%% 
%%% gen_server Module callback functions
%%%

init(DbConfig) ->
	%receive X -> X after 5000 -> timeout end,
	case epgsql:connect(DbConfig) of
			{ok, C} ->
				{ok, C};
			{error, R} ->
				{stop, R}
	end.

terminate(_Reason, C) ->
	io:format("[DEBUG] db:terminate/2 : terminating db~n"),
	epgsql:close(C).

handle_call({get_event, RoomId, EventId}, _From, C) ->
	{reply, select_event(C, RoomId, EventId), C};

handle_call({get_messages, RoomId, Qs}, _From, C) ->
	Limit = maps:get(limit, Qs),
	{reply, select_messages(C, RoomId, Limit), C};

handle_call({send_message, Message, RoomId, Sender, TxdId}, _From, C) -> 
	{reply, insert_message(C, Message, RoomId, Sender, TxdId), C}.

handle_cast(_, C) ->
		{noreply, C}.
	

%%% 
%%% Internals
%%%

%% @doc Queries a database for all messages that match the Arguments
-spec select_messages(C :: epgsql:connection(), RoomId :: room_id(), Limit :: integer()) -> table() | {error, any()}.
select_messages(C, RoomId, Limit) ->
		case epgsql:equery(C, "SELECT content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key FROM Events WHERE room_id = $1 ORDER BY depth DESC LIMIT $2", [RoomId, Limit]) of
				{ok, Cols, Rows} ->
						{ok, table_to_list(Cols, Rows)};
				{error, Reason} ->
						{error, Reason}
		end.

%% @doc Queries database for a single event, but returns a list
-spec select_event(C :: epgsql:connection(), RoomId :: room_id(), EventId :: event_id() ) -> table() | {error, any()}.
select_event(C, RoomId, EventId) ->
	case epgsql:equery(C, "SELECT * FROM Events WHERE room_id = $1 AND event_id = $2;", [RoomId, EventId]) of
		{ok, Cols, Rows} -> 
			{ok, table_to_list(Cols, Rows)};
		%{ok, _, []} ->
			%{reply, {error, not_found}, C};
		{error, Reason} ->
			{error, Reason}
	end.


%% @doc Converts a table into a list of maps. Given a column and a corresponding list of row tuples
%% as returned by {@link epgsql:squery/2. epgsql:squery/2}, this function creates a list of maps for every row
%% where the key is the name of a column and the value the corresponding row element. The output of this
%% function is intended to be parsed to json.
%% @end

-spec table_to_list(Cols :: [#column{}], [] | [tuple()]) -> table().

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


%% @doc Inserts a m.room.message event into the Event table.
-spec insert_message(C :: epgsql:connection(), Body :: binary(), RoomId :: binary(), Sender :: binary(), TxdId :: binary()) -> {ok, binary()} | {error, unknown_room}.

insert_message(C, Message, RoomId, Sender, _TxdId) ->
	case epgsql:equery(C, "SELECT depth FROM events WHERE room_id=$1 ORDER BY depth DESC LIMIT 1;", [RoomId]) of
		{ok,[#column{name = <<"depth">>}],[{LastDepth}]} ->
					Depth = LastDepth + 10,
					Body = jiffy:encode(#{msgtype => <<"m.text">>, body => Message}),
					{ok, 1, _, [{Eid}]} = epgsql:equery(C, 
									"INSERT INTO Events (content, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES ($1, $2, $3, $4, 'm.room.message', '{}', ' ', $5) returning event_id;", 
									[Body, os:system_time(microsecond), RoomId, Sender, Depth]),
					{ok, Eid}; 
		{ok,_,[]} -> {error, unknown_room}
	end.
