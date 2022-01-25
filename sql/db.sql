DROP TABLE IF EXISTS RoomEvent;
CREATE TABLE IF NOT EXISTS RoomEvent (
	content jsonb NOT NULL,
	event_id text PRIMARY KEY,
	origin_server_ts BIGINT NOT NULL ,
	room_id text NOT NULL,
	sender text NOT NULL,
	type text NOT NULL,
	unsigned jsonb
);

INSERT INTO RoomEvent (content, event_id, origin_server_ts, room_id, sender, type, unsigned) VALUES
('{"body": "hello world", "msgtype": "m.text"}',
  'ffaa', 1638547064954, '!acex:local.org', '@alice:local.org', 'm.room.message', '{"age": 4046466692}'),
('{"body": "second message", "msgtype": "m.text"}',
  'fadxa', 1638547064954, '!acex:local.org', '@bob:local.org', 'm.room.message', '{"age": 4046466582}');

