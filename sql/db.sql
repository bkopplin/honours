DROP TABLE IF EXISTS Events;
CREATE TABLE IF NOT EXISTS Events (
	content jsonb NOT NULL,
	event_id text PRIMARY KEY,
	origin_server_ts BIGINT NOT NULL ,
	room_id text NOT NULL,
	sender text NOT NULL,
	type text NOT NULL,
	unsigned jsonb,
	state_key text DEFAULT NULL,
	prev_content jsonb DEFAULT NULL
);

INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned) VALUES
('{"body": "hello world", "msgtype": "m.text"}',
  'eid1', 1638547064954, '!r1:localhost', '@alice:localhost', 'm.room.message', '{"age": 4046466692}'),
('{"body": "second message", "msgtype": "m.text"}',
  'eid2', 1638547064954, '!r1:localhost', '@bob:localhost', 'm.room.message', '{"age": 4046466582}');

INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key) VALUES
('{"creator": "@alice:localhost","room_version": "6"}', 'eid0', 1635938428328, '!r1:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', ' ');
