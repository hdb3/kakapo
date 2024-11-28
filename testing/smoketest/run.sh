#!/bin/bash -e
: '
directory structure
kakapo
-- testing      - PARENT_DIR
-- -- bin       - BIN_DIR
-- -- smoketest - BASE_DIR
-- -- -- conf   - BASE_DIR/conf
'
BASE_DIR=$(realpath $(dirname "$0"))
PROG=${1-"bird2"}
CONFIG="$BASE_DIR/conf/$PROG.conf"
SMOKETEST_DIR=$(realpath "$BASE_DIR/..")
PARENT_DIR=$(realpath "$SMOKETEST_DIR/..")
BIN_DIR="$SMOKETEST_DIR/bin"

set_command() {
	case $1 in

	kakapo) COMMAND="$PARENT_DIR/core/kakapo 172.18.0.13,172.18.0.19,64505 172.18.0.13,172.18.0.20,64504" ;;

	bgpd) COMMAND="$BIN_DIR/bgpd/bgpd -d -f ${CONFIG}" ;;

	bird) COMMAND="$BIN_DIR/bird/bird -d -c ${CONFIG}" ;;

	bird2)
		mkdir -p /run/bird
		COMMAND="$BIN_DIR/bird2/bird -d -c ${CONFIG}"
		;;

	frr) COMMAND="$BIN_DIR/frr/bgpd --pid_file=frr.pid --skip_runas --listenon=172.18.0.13 --no_zebra --log-level=debug --log=stdout --config_file ${CONFIG}" ;;

	gobgpd) COMMAND="$BIN_DIR/gobgp/gobgpd --log-plain --config-file=${CONFIG}" ;;

	*)
		echo "unknown daemon: $1"
		exit 1
		;;
	esac
	echo ${COMMAND}
}

pkill() {
	case $1 in
	kakapo) : ;;
	bgpd | frr) sudo killall bgpd ;;
	bird | bird2) sudo killall bird ;;
	gobgpd) sudo killall gobgpd ;;

	*)
		echo "unknown daemon: $1"
		exit 1
		;;
	esac
	echo ${COMMAND}
}

if [[ -f "$CONFIG" ]]; then
	CMND=$(set_command $1)
	echo "command is: \"$CMND\""
	sudo -b ${CMND}
	sudo $(set_command "kakapo")
	wait
	echo "all done"
	pkill $1
else
	echo "can't run, $CONFIG not exists"
fi
