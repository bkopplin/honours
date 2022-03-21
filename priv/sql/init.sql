DROP TABLE IF EXISTS Events;
CREATE TABLE IF NOT EXISTS Events (
	content jsonb NOT NULL,
	event_id SERIAL UNIQUE,
	origin_server_ts BIGINT NOT NULL ,
	room_id text NOT NULL,
	sender text NOT NULL,
	type text NOT NULL,
	unsigned jsonb,
	state_key text DEFAULT NULL,
	prev_content jsonb DEFAULT NULL, -- potentially being used in unsigned field
	depth INT NOT NULL 
);

DROP TABLE IF EXISTS Users;
CREATE TABLE IF NOT EXISTS Users (
	user_id text UNIQUE,
	is_guest BOOLEAN NOT NULL,
	password text NOT NULL
);
