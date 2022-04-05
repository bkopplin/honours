DELETE FROM Events WHERE 1=1;
DELETE FROM Users WHERE 1=1;
DELETE FROM Sessions WHERE 1=1;

-- room r0
INSERT INTO Events (event_id, content, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES
('$6xBhAM+48cxmjCMYvicxGHbw7O44', '{"creator": "@alice:localhost","room_version": "6"}', 1635938428328, '!r0:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', ' ', 1),
('$0a8t4g+1zyM5z3764wx7lty5y285', '{"body": "hello world", "msgtype": "m.text"}',
1638547064954, '!r0:localhost', '@alice:localhost', 'm.room.message', '{"age": 4046466692}', NULL, 11),
('$ybLk0ybuf/miPqBEedrYSKqHFhM9', '{"body": "second message", "msgtype": "m.text"}',
 1638547064954, '!r0:localhost', '@bob:localhost', 'm.room.message', '{"age": 4046466582}', NULL, 21);

-- room r2 
INSERT INTO Events (event_id, content, origin_server_ts, room_id, sender, type, unsigned, state_key, depth) VALUES
('$JIs4uy4/nhYFPfM847nNL5uofM4s', '{"creator": "@alice:localhost","room_version": "6"}', 1635938428328, '!r2:localhost', '@alice:localhost', 'm.room.create', '{"age": 7773042179}', '', 1),
('$XAiwlghOtg/IGYMkQ+9fOdcBHm0j', '{"displayname": "devbk", "membership": "join" }', 1643802066969, '!r2:localhost', '@alice:localhost', 'm.room.member', '{"age":9245454}', '', 11),
('$rGBwLmspGdy5k/Akzrlu3decWIgj', '{
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
('$oReVWNbVATTcmASwQWK5bb825A00', '{"join_rule": "invite"}',  1643802067204, '!r2:localhost', '@alice:localhost', 'm.room.join_rules', '{"age":9245219}', '', 31),
('$irJZYU1fU/0xuzOtEuGAkx7R8c43', '{"history_visibility":"shared"}', 1643802067297, '!r2:localhost', '@alice:localhost', 'm.room.history_visibility', '{"age":9245126}', '', 41),
('$dHfYhgn3vfTIJAJQziSAFOFVFqc3', '{"guest_access": "can_join"}', 1643802067569, '!r2:localhost', '@alice:localhost', 'm.room.guest_access', '{"age":9244854}', '', 51),
('$+BTSW0jUIJ5wmm+cbOeynfci0xAd', '{"name":"devbk_1"}', 1643802067738, '!r2:localhost', '@alice:localhost', 'm.room.name', '{"age":9244685}', '', 61),
('$eJFo98MW4nm3ivyPxoIDCx6sj80j', '{"body":"hello world", "msgtype":"m.text"}', 1643802450579, '!r2:localhost', '@alice:localhost', 'm.room.message', '{"age":8861844}', '', 71),
('$MGseDmxWUzk7jOCGV0989qp47kck', '{"name":"devbk_0"}', 1643802600941, '!r2:localhost', '@alice:localhost', 'm.room.name', '{"age":8711482,"replaces_state":"edi6","prev_content":{"name":"devbk_1"}, "prev_sender":"@alice:localhost"}', '', 81);

Insert INTO Users (user_id, password) VALUES
('@neo:localhost', 'bgDNVizC2I4jjfuB2UOd5+yEPunQyYedVJyxQ2eG+XU='), -- matrix
('@bob:localhost', 'XohImNooBHFR0OVvjcYpJ3NgPQ1qq73WKhHvch0VQtg='); -- password

INSERT INTO Sessions (user_id, token, device_id) VALUES
('@neo:localhost', '1385540A7F7C5D4C55424B2E00C150135C0FD213B31C424030C4851ECA1B', '3CF6EF26115EEE5634D75194A0528340BB85CA97'),
('@bob:localhost', '0213FE565EC1DDDDD7C37539C1DA22CEF0050B6E1F093758A428FAAED1DC', '9238C654A1A9DB7DD561A8890B7025E2704561F');
