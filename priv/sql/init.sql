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

