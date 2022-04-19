if [ $# -lt 1 ]; then
	echo "run as: ${0} prod"
	exit
fi
if [ $1 = "prod" ]; then
	rebar3 as prod release 
	docker build -t bkopplin/eneo:manifest-$(dpkg --print-architecture) .
	docker push bkopplin/eneo:manifest-$(dpkg --print-architecture)
fi


