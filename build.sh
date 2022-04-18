if [ $# -lt 1 ]; then
	echo "run as: ${0} prod"
	exit
fi
if [ $1 = "prod" ]; then
	rebar3 as prod release 
	docker build -t bkopplin/eneo:latest .
	docker push bkopplin/eneo:latest
fi


