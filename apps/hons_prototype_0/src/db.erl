-module(db).

-export([get_event/2]).

get_event(RoomId, EventId) ->
	Event = #{
	  <<"content">> => #{
	      <<"body">> => <<"a message from bjarne">>,
	      <<"msgtype">> => <<"m.text">>
	     },
	  <<"origin_server_ts">> => 1638547077502,
	  <<"room_id">> => RoomId,
	  <<"sender">> => <<"@noutria:matrix.org">>,
	  <<"type">> => <<"m.room.message">>,
	  <<"unsigned">> => #{
	      <<"age">> => 4044094843
	     },
	  <<"event_id">> => EventId,
	  <<"user_id">> => <<"@noutria:matrix.org">>,
	  <<"age">> => 4044094843
	 },
	Event.
