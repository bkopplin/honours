-- to keep things simple a sequence generator is used to generate the next depth value
-- when using federation it is expected that the depth value creation will need to be 
-- handled by application code

DROP SEQUENCE IF EXISTS depth_seq;
CREATE SEQUENCE IF NOT EXISTS depth_seq INCREMENT BY 10; 

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
	prev_content jsonb DEFAULT NULL, -- potentially being used in unsigned field
	depth INT NOT NULL DEFAULT nextval('depth_seq') 
);

INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned) VALUES
('{"body": "hello world", "msgtype": "m.text"}',
  'eid1', 1638547064954, '!r1:localhost', '@alice:localhost', 'm.room.message', '{"age": 4046466692}'),
('{"body": "second message", "msgtype": "m.text"}',
  'eid2', 1638547064954, '!r1:localhost', '@bob:localhost', 'm.room.message', '{"age": 4046466582}');

INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key) VALUES
('{"creator": "@alice:localhost","room_version": "6"}', 'eid0', 1635938428328, '!r1:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', ' ');
