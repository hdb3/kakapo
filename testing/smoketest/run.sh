#!/bin/bash -e
: '
directory structure
kakapo          - KAKAPO_DIR
-- testing      - TESTING_DIR
-- -- bin       - BIN_DIR
-- -- smoketest - SCRIPT_DIR
-- -- -- conf   - SCRIPT_DIR/conf
'

PROG=${1-"bird2"} #  default target: bird2
EXTRA_PARAMETERS="${*:2}"
TINY_GROUPS="GROUPSIZE=1  TABLESIZE=2 MAXBURSTCOUNT=2"
SMALL_GROUPS="GROUPSIZE=10  TABLESIZE=100000 MAXBURSTCOUNT=80000"
LARGE_GROUPS="GROUPSIZE=800 TABLESIZE=100000 MAXBURSTCOUNT=1000"
DEFAULTS="LOGTEXT=\"$PROG $EXTRA_PARAMETERS\" REPEAT=1 TABLESIZE=100000"

MULTI_LARGE="$DEFAULTS $LARGE_GROUPS MODE=MULTI"
SINGLE_LARGE="$DEFAULTS $LARGE_GROUPS MODE=SINGLEONLY"

MULTI_SMALL="$DEFAULTS $SMALL_GROUPS MODE=MULTI"
SINGLE_SMALL="$DEFAULTS $SMALL_GROUPS MODE=SINGLEONLY"
SINGLE_TINY="$DEFAULTS $TINY_GROUPS MODE=SINGLEONLY"
RATE="$DEFAULTS $SMALL_GROUPS RATECOUNT=10000000 MODE=SINGLERATE"

# most common variant, replace _SMALL with _LARGE,
# and change value of REPEAT
KAKAPO_ENV="$SINGLE_LARGE REPEAT=10"
# KAKAPO_ENV="$RATE"

SCRIPT_DIR=$(realpath $(dirname "$0"))
CONFIG="$SCRIPT_DIR/conf/$PROG.conf"
TESTING_DIR=$(realpath "$SCRIPT_DIR/..")
KAKAPO_DIR=$(realpath "$TESTING_DIR/..")
KAKAPO_BIN=${OVERRIDE:-"$KAKAPO_DIR/core/kakapo"}
BIN_DIR="$TESTING_DIR/bin"

set_command() {
	local COMMAND
	case $1 in

	kakapo) COMMAND="${KAKAPO_ENV} $KAKAPO_BIN 172.18.0.13,172.18.0.19,64505 172.18.0.13,172.18.0.20,64504" ;;

	hbgp) COMMAND="$BIN_DIR/hbgp/hbgp ${CONFIG}" ;;

	bgpd) COMMAND="$BIN_DIR/bgpd/bgpd -d -f ${CONFIG}" ;;

	bird) COMMAND="$BIN_DIR/bird/bird -d -c ${CONFIG}" ;;

	bird2)
		sudo mkdir -p /run/bird
		COMMAND="$BIN_DIR/bird2/bird -d -c ${CONFIG}"
		;;

	frr) COMMAND="$BIN_DIR/frr/bgpd --pid_file=frr.pid --skip_runas --listenon=172.18.0.13 --no_zebra --log-level=debug --log=stdout --config_file ${CONFIG}" ;;

	gobgp) COMMAND="$BIN_DIR/gobgp/gobgpd --log-plain --config-file=${CONFIG}" ;;

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
	bgpd | frr) kill9 bgpd ;;
	bird | bird2) kill9 bird ;;
	gobgp) kill9 gobgpd ;;
	hbgp) kill9 hbgp ;;
	relay) kill9 relay2 ;;
	libvirt) : ;;

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

if [[ -f "$CONFIG" ]]; then
	CMND=$(set_command $1)
	# echo "command is: \"$CMND\""
	PIDFILE=$(mktemp)
	bash -c "${CMND} & echo \$! > $PIDFILE"
	PID=$(<$PIDFILE)
	sleep 2.0
	# echo "COMMAND = $(set_command "kakapo")"
	eval $(set_command "kakapo")
	kill $PID
	pkill $1
else
	echo "can't run, $CONFIG not exists"
fi
