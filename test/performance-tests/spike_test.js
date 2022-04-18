import http from 'k6/http';
import { check, sleep } from 'k6';

export var BASE_URL='http://' + __ENV.HOST;

export const options = {
		stages: [
			{ duration: '10s', target: 100 },
			{ duration: '2m', target: 100 },
			{ duration: '10s', target: 1000 },
			{ duration: '10m', target: 1000 },
			{ duration: '10s', target: 100 },
			{ duration: '1m', target: 100 },
			{ duration: '5s', target: 0 },
		]
};

export default function() {
		  const responses = http.batch([
			['GET', `${BASE_URL}/rooms/!r1:localhost/messages/`],
			['GET', `${BASE_URL}/account/login/`],
		  ]);
		sleep(0.0001);
}
