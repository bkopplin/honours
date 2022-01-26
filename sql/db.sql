DROP TABLE IF EXISTS RoomEvents;
CREATE TABLE IF NOT EXISTS RoomEvents (
	content jsonb NOT NULL,
	event_id text PRIMARY KEY,
	origin_server_ts BIGINT NOT NULL ,
	room_id text NOT NULL,
	sender text NOT NULL,
	type text NOT NULL,
	unsigned jsonb
);

INSERT INTO RoomEvents (content, event_id, origin_server_ts, room_id, sender, type, unsigned) VALUES
('{"body": "hello world", "msgtype": "m.text"}',
  'eid1', 1638547064954, '!r1:localhost', '@alice:localhost', 'm.room.message', '{"age": 4046466692}'),
('{"body": "second message", "msgtype": "m.text"}',
  'eid2', 1638547064954, '!r1:localhost', '@bob:localhost', 'm.room.message', '{"age": 4046466582}');

