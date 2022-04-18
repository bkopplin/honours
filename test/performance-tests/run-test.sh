if [  $# != 3 ]; then
	echo "Runs load tests and saves them to the correct path"
	echo "${0} <dir> <filename> <test-script>"
	exit 1
fi
OUTDIR=$1
FILE=$2
SCRIPT=$3

k6 run --env HOST='127.0.0.1:8081' $SCRIPT | tee /dev/tty | tee ${OUTDIR}/${FILE}.txt
