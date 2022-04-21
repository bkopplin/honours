import http from 'k6/http';
import { check, sleep } from 'k6';

export const options = {
		stages: [
				{duration: '1m', target: 600},
				{duration: '2m', target: 600},
				{duration: '1m', target: 0},
		]
};

export default function() {
		const res = http.get('http://' + __ENV.HOST + '/rooms/!r0:localhost/messages');
		console.log("body:" , res.body);
		console.log("code:" , res.status);
		check(res, { 
				'status was 200': (r) => r.status == 200,
				'number of events was 3': (res) => res.json().length == 3, 
		});
		sleep(0.0001);
}
