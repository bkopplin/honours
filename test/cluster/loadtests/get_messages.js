import http from 'k6/http';
import { check, sleep } from 'k6';

const host = 'http://127.23.0.1:8081';

export const options = {
		stages: [
				{duration: '60s', target: 20},
		]
};

export default function() {
		const res = http.get(host + '/rooms/!r1:localhost/messages');
		check(res, { 
				'status was 200': (r) => r.status == 200,
				'number of events was 3': (res) => res.json().length == 3, 
		});
		sleep(0.0001);
}
