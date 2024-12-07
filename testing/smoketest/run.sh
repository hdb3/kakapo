#!/bin/bash -e
: '
directory structure
kakapo          - KAKAPO_DIR
-- testing      - TESTING_DIR
-- -- bin       - BIN_DIR
-- -- smoketest - SCRIPT_DIR
-- -- -- conf   - SCRIPT_DIR/conf
'

PROG=${1-"bird2"} # run bird as default target
# shift
EXTRA_PARAMETERS="${*:2}" # https://unix.stackexchange.com/questions/197792/joining-bash-arguments-into-single-string-with-spaces
SMALL_GROUPS="GROUPSIZE=10  TABLESIZE=100000 MAXBURSTCOUNT=80000"
LARGE_GROUPS="GROUPSIZE=800 TABLESIZE=100000 MAXBURSTCOUNT=1000"
DEFAULTS="LOGTEXT='$PROG $EXTRA_PARAMETERS' REPEAT=1 TABLESIZE=100000"

MULTI_LARGE="$DEFAULTS $LARGE_GROUPS MODE=MULTI"
SINGLE_LARGE="$DEFAULTS $LARGE_GROUPS MODE=SINGLEONLY"

MULTI_SMALL="$DEFAULTS $SMALL_GROUPS MODE=MULTI"
SINGLE_SMALL="$DEFAULTS $SMALL_GROUPS MODE=SINGLEONLY"
KAKAPO_ENV="$SINGLE_LARGE REPEAT=100"

# KAKAPO_ENV="LOGTEXT=$PROG REPEAT=1 MODE=SINGLEONLY GROUPSIZE=10  TABLESIZE=100000 MAXBURSTCOUNT=80000 MAXBLOCKINGFACTOR=1"
# KAKAPO_ENV="LOGTEXT=$PROG REPEAT=1 MODE=SINGLEONLY GROUPSIZE=800 TABLESIZE=100000 MAXBURSTCOUNT=1000  MAXBLOCKINGFACTOR=1"
SCRIPT_DIR=$(realpath $(dirname "$0"))
CONFIG="$SCRIPT_DIR/conf/$PROG.conf"
TESTING_DIR=$(realpath "$SCRIPT_DIR/..")
KAKAPO_DIR=$(realpath "$TESTING_DIR/..")
BIN_DIR="$TESTING_DIR/bin"

set_command() {
	local COMMAND
	case $1 in

	# kakapo) COMMAND="${KAKAPO_ENV} $KAKAPO_DIR/oldcore/kakapo 172.18.0.13,172.18.0.19,64505 172.18.0.13,172.18.0.20,64504" ;;
	kakapo) COMMAND="${KAKAPO_ENV} $KAKAPO_DIR/core/kakapo 172.18.0.13,172.18.0.19,64505 172.18.0.13,172.18.0.20,64504" ;;

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

	*)
		echo "unknown daemon: $1"
		exit 1
		;;
	esac
	echo ${COMMAND}
}

if [[ -f "$CONFIG" ]]; then
	CMND="ip netns exec target $(set_command $1)"
	echo "command is: \"$CMND\""
	${CMND} &
	PID=$!
	sleep 2.0
	KCMND=$(set_command "kakapo")
	echo "command is: ip netns exec kakapo $KCMND"
	eval "ip netns exec kakapo /bin/bash -c \"$KCMND\""
	# echo "all done"
	kill $PID
	pkill $1
else
	echo "can't run, $CONFIG not exists"
fi
