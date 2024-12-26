#!/bin/bash -e
#shellcheck disable=SC2034,SC2046,SC2089,SC2090
: '
directory structure
kakapo          - KAKAPO_DIR
-- testing      - TESTING_DIR
-- -- bin       - BIN_DIR
-- -- smoketest - SCRIPT_DIR
-- -- -- conf   - SCRIPT_DIR/conf
'
export DOCKER_BUILDKIT=1 DOCKER_HOST=127.0.0.1

get_psu_status() {
	if [[ ! -f "/sys/class/power_supply/AC/online" ]]; then
		echo "AC"
	else
		PSU_STATUS=$(</sys/class/power_supply/AC/online)
		case $PSU_STATUS in
		0) echo "BATTERY" ;;
		1) echo "AC" ;;
		*) echo "UNKNOWN" ;;
		esac
	fi
}
PSU_STATUS=$(get_psu_status)

RATETIME=3600

# PERFWRAPPER="LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libc_malloc_debug.so.0"
# PERFWRAPPER="perf record -g -F 99"
PERFWRAPPER=""

PROG=${1-"bird2"} # default target: bird2
EXTRA_PARAMETERS="${*:2}"
TINY_GROUPS="GROUPSIZE=1 TABLESIZE=2 MAXBURSTCOUNT=2"
SMALL_GROUPS="GROUPSIZE=10 TABLESIZE=100000 MAXBURSTCOUNT=80000"
LARGE_GROUPS="GROUPSIZE=800 TABLESIZE=100000 MAXBURSTCOUNT=1000"
DEFAULTS="LOGTEXT=\"$PROG PSU_STATUS=$PSU_STATUS $EXTRA_PARAMETERS\" TIMEOUT=20 REPEAT=1 TABLESIZE=100000"

MULTI_LARGE="$DEFAULTS $LARGE_GROUPS MODE=MULTI"
SINGLE_LARGE="$DEFAULTS $LARGE_GROUPS MODE=SINGLEONLY"

MULTI_SMALL="$DEFAULTS $SMALL_GROUPS MODE=MULTI"
SINGLE_SMALL="$DEFAULTS $SMALL_GROUPS MODE=SINGLEONLY"
SINGLE_TINY="$DEFAULTS $TINY_GROUPS MODE=SINGLEONLY"
RATE="$DEFAULTS $SMALL_GROUPS WINDOW=5000 RATETIMELIMIT=$RATETIME MODE=SINGLERATE"

# most common variant, replace _SMALL with _LARGE,
# and/or, change value of REPEAT,
# or, switch to RATE measurement
KAKAPO_ENV="$SINGLE_LARGE REPEAT=10"
# KAKAPO_ENV="$RATE"

SCRIPT_DIR=$(realpath $(dirname "$0"))
CONFIG="$SCRIPT_DIR/conf/$PROG.conf"
TESTING_DIR=$(realpath "$SCRIPT_DIR/..")
KAKAPO_DIR=$(realpath "$TESTING_DIR/..")
KAKAPO_BIN=${OVERRIDE:-"$PERFWRAPPER $KAKAPO_DIR/core/kakapo"}
BIN_DIR="$TESTING_DIR/bin"

DOCKER_RUN="docker run --rm --cap-add NET_ADMIN --cap-add SYS_ADMIN --network host"

set_command() {
	local COMMAND
	case $1 in

	kakapo) COMMAND="${KAKAPO_ENV} $KAKAPO_BIN 172.18.0.13,172.18.0.19,64505 172.18.0.13,172.18.0.20,64504" ;;

	bgpd | bird | bird2 | gobgp | hbgp) COMMAND="$DOCKER_RUN --volume ${CONFIG}:/config/bgpd.conf --name $1 $1" ;;

	frr) COMMAND="$DOCKER_RUN --env BGPLISTENADDR=172.18.0.13 --volume ${CONFIG}:/config/bgpd.conf --name $1 $1" ;;

	gobgpV2) COMMAND="$BIN_DIR/gobgp/gobgpd --log-plain --config-file=${CONFIG}" ;;

	relay) COMMAND="$KAKAPO_DIR/relay/relay2 172.18.0.13 172.18.0.19" ;;

	libvirt) COMMAND="echo \"check VM started...\"" ;;

	esac

	if [[ -z "${COMMAND}" ]]; then
		echo "unknown daemon: $1"
		exit 1
	fi

	echo ${COMMAND}
}

kill9() {
	killall -9 "$1" &>/dev/null
}

pkill() {
	case $1 in
	kakapo) : ;;
	hbgp | bgpd | gobgp | bird2 | bird | frr) docker kill $1 ;;
	relay) kill9 relay2 ;;
	libvirt) : ;;
	gobgpV2) kill9 gobgpd ;;

	*)
		echo "unknown daemon: $1"
		exit 1
		;;
	esac
	echo ${COMMAND}
}

$TESTING_DIR/netns.sh del &>/dev/null || :

if [[ "$1" == "libvirt" ]]; then
	$SCRIPT_DIR/add_loopbacks.sh del &>/dev/null
	$SCRIPT_DIR/add_loopbacks.sh add virbr1 &>/dev/null
else
	$SCRIPT_DIR/add_loopbacks.sh del virbr1 &>/dev/null
	$SCRIPT_DIR/add_loopbacks.sh &>/dev/null
fi

docker kill bird bird2 relay gobgp hbgp frr bgpd &>/dev/null || :

if [[ -f "$CONFIG" ]]; then
	CMND=$(set_command $1)
	# echo "command is: \"$CMND\""
	PIDFILE=$(mktemp)
	bash -c "${CMND} & echo \$! > $PIDFILE"
	PID=$(<$PIDFILE)
	sleep 2.0
	# echo "COMMAND = $(set_command "kakapo")"
	eval $(set_command "kakapo")
	pkill $1
else
	echo "can't run, $CONFIG not exists"
fi
