if [  $# != 3 ]; then
	echo "Runs load tests and saves them to the correct path"
	echo "${0} <dir> <filename> <test-script>"
fi
OUTDIR=$1
FILE=$2
SCRIPT=$3

k6 run $SCRIPT | tee /dev/tty | > ${OUTDIR}/full/${FILE}.txt |  grep -e checks -e http_req_duration > ${OUTDIR}/filtered/${FILE}.txt

