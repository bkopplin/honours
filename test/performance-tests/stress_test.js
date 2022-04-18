import http from 'k6/http';
import { check, sleep } from 'k6';

export var BASE_URL='http://' + __ENV.HOST

export const options = {
		stages: [
			{ duration: '1m', target: 100 },
			{ duration: '3m', target: 100 },
			{ duration: '1m', target: 200 },
			{ duration: '3m', target: 200 },
			{ duration: '1m', target: 300 },
			{ duration: '3m', target: 300 },
			{ duration: '1m', target: 400 },
			{ duration: '3m', target: 400 },
			{ duration: '5m', target: 0 },
		]
};

export default function() {
		  const responses = http.batch([
			['GET', `${BASE_URL}/rooms/!r1:localhost/messages/`],
			['GET', `${BASE_URL}/login/`],
		  ]);
		sleep(0.0001);
}
