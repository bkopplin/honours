-- room r0
INSERT INTO Events (content, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES
('{"creator": "@alice:localhost","room_version": "6"}', 1635938428328, '!r0:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', ' ', 1),
('{"body": "hello world", "msgtype": "m.text"}',
1638547064954, '!r0:localhost', '@alice:localhost', 'm.room.message', '{"age": 4046466692}', NULL, 11),
('{"body": "second message", "msgtype": "m.text"}',
 1638547064954, '!r0:localhost', '@bob:localhost', 'm.room.message', '{"age": 4046466582}', NULL, 21);

-- room r2 
INSERT INTO Events (content, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES
('{"creator": "@alice:localhost","room_version": "6"}', 1635938428328, '!r2:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', '', 1),
('{"displayname": "devbk", "membership": "join" }', 1643802066969, '!r2:localhost', '@alice:localhost', 'm.room.member', '{"age":9245454}', '', 11),
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
        "users_default": 0}', 1643802067109, '!r2:localhost', '@alice:localhost', 'm.room.power_levels', '{"age":9245314}', '', 21),
('{"join_rule": "invite"}',  1643802067204, '!r2:localhost', '@alice:localhost', 'm.room.join_rules', '{"age":9245219}', '', 31),
('{"history_visibility":"shared"}', 1643802067297, '!r2:localhost', '@alice:localhost', 'm.room.history_visibility', '{"age":9245126}', '', 41),
('{"guest_access": "can_join"}', 1643802067569, '!r2:localhost', '@alice:localhost', 'm.room.guest_access', '{"age":9244854}', '', 51),
('{"name":"devbk_1"}', 1643802067738, '!r2:localhost', '@alice:localhost', 'm.room.name', '{"age":9244685}', '', 61),
('{"body":"hello world", "msgtype":"m.text"}', 1643802450579, '!r2:localhost', '@alice:localhost', 'm.room.message', '{"age":8861844}', '', 71),
('{"name":"devbk_0"}', 1643802600941, '!r2:localhost', '@alice:localhost', 'm.room.name', '{"age":8711482,"replaces_state":"edi6","prev_content":{"name":"devbk_1"}, "prev_sender":"@alice:localhost"}', '', 81);

