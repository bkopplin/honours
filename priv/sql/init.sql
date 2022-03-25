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
	password text NOT NULL,
	is_guest BOOLEAN NOT NULL
);

DROP TABLE IF EXISTS Sessions;
CREATE TABLE IF NOT EXISTS Sessions (
	user_id text NOT NULL,
	token text NOT NULL,
	device_id text NOT NULL UNIQUE
);

DROP TABLE IF EXISTS Devices;
CREATE TABLE IF NOT EXISTS Devices (
	device_id text UNIQUE,
	display_name text NOT NULL
);
