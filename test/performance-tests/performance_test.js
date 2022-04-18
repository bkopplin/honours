import http from 'k6/http';
import { check, sleep } from 'k6';

export const options = {
		stages: [
				{duration: '2m', target: 100},
				{duration: '4m', target: 100},
				{duration: '2m', target: 0},
		]
};

export default function() {
		const res = http.get('http://' + __ENV.HOST + '/rooms/!r1:localhost/messages');
		check(res, { 
				'status was 200': (r) => r.status == 200,
				'number of events was 3': (res) => res.json().length == 3, 
		});
		sleep(0.0001);
}
