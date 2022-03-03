import http from 'k6/http';
import { check, sleep } from 'k6';

const host = 'http://127.23.0.1:8081';
//const host = 'http://docker_loadbalancer_1';

export const options = {
		stages: [
				{duration: '10s', target: 20},
		]
};

export default function() {
		const res = http.get(host + '/rooms/!r1:localhost/messages');
		check(res, { 'status was 200': (r) => r.status == 200});
		sleep(0.001);
}
