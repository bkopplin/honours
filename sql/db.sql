-- to keep things simple a sequence generator is used to generate the next depth value
-- when using federation it is expected that the depth value creation will need to be 
-- handled by application code
-- 03.02.2022: using a sequence to calculate depth is unsuatable because depth starts at
-- 1 for every room but a sequence is continuous for an entire table

DROP SEQUENCE IF EXISTS depth_seq;
CREATE SEQUENCE IF NOT EXISTS depth_seq INCREMENT BY 10; 

DROP TABLE IF EXISTS Events;
CREATE TABLE IF NOT EXISTS Events (
	content jsonb NOT NULL,
	event_id text NOT NULL,
	origin_server_ts BIGINT NOT NULL ,
	room_id text NOT NULL,
	sender text NOT NULL,
	type text NOT NULL,
	unsigned jsonb,
	state_key text DEFAULT NULL,
	prev_content jsonb DEFAULT NULL, -- potentially being used in unsigned field
	depth INT NOT NULL 
);

INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES
('{"creator": "@alice:localhost","room_version": "6"}', 'eid0', 1635938428328, '!r1:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', ' ', 1),
('{"body": "hello world", "msgtype": "m.text"}',
  'eid1', 1638547064954, '!r1:localhost', '@alice:localhost', 'm.room.message', '{"age": 4046466692}', NULL, 11),
('{"body": "second message", "msgtype": "m.text"}',
  'eid2', 1638547064954, '!r1:localhost', '@bob:localhost', 'm.room.message', '{"age": 4046466582}', NULL, 21);

-- room r2 
INSERT INTO Events (content, event_id, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES
('{"creator": "@alice:localhost","room_version": "6"}', 'eid0', 1635938428328, '!r2:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', '', 1),
('{"displayname": "devbk", "membership": "join" }', 'eid1', 1643802066969, '!r2:localhost', '@alice:localhost', 'm.room.member', '{"age":9245454}', '', 11),
('{
        "ban": 50,
        "events": {
          "m.room.avatar": 50,
          "m.room.canonical_alias": 50,
          "m.room.encryption": 100,
          "m.room.history_visibility": 100,
          "m.room.name": 50,
          "m.room.power_levels": 100,
          "m.room.server_acl": 100,
          "m.room.tombstone": 100
        },
        "events_default": 0,
        "historical": 100,
        "invite": 0,
        "kick": 50,
        "redact": 50,
        "state_default": 50,
        "users": {
          "@alice:localhost": 100
        },
        "users_default": 0}', 'eid2', 1643802067109, '!r2:localhost', '@alice:localhost', 'm.room.power_levels', '{"age":9245314}', '', 21),
('{"join_rule": "invite"}', 'eid3', 1643802067204, '!r2:localhost', '@alice:localhost', 'm.room.join_rules', '{"age":9245219}', '', 31),
('{"history_visibility":"shared"}', 'eid4', 1643802067297, '!r2:localhost', '@alice:localhost', 'm.room.history_visibility', '{"age":9245126}', '', 41),
('{"guest_access": "can_join"}', 'eid5', 1643802067569, '!r2:localhost', '@alice:localhost', 'm.room.guest_access', '{"age":9244854}', '', 51),
('{"name":"devbk_1"}', 'eid6', 1643802067738, '!r2:localhost', '@alice:localhost', 'm.room.name', '{"age":9244685}', '', 61),
('{"body":"hello world", "msgtype":"m.text"}', 'eid7', 1643802450579, '!r2:localhost', '@alice:localhost', 'm.room.message', '{"age":8861844}', '', 71),
('{"name":"devbk_0"}', 'eid8', 1643802600941, '!r2:localhost', '@alice:localhost', 'm.room.name', '{"age":8711482,"replaces_state":"edi6","prev_content":{"name":"devbk_1"}, "prev_sender":"@alice:localhost"}', '', 81);

