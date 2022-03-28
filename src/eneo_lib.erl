-module(eneo_lib).

-export([gen_access_token/0, gen_device_id/0]).

gen_access_token() ->
	base64:encode(crypto:strong_rand_bytes(30)).

gen_device_id() ->
	base64:encode(crypto:strong_rand_bytes(20)).
