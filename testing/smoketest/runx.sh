#!/bin/bash -e
#shellcheck disable=SC2034,SC2046,SC2089,SC2090

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

: "${TAG:=\"UNSET\"}"
: "${SPEC:=\"UNSET\"}"
: "${MODE:=\"RATE\"}"
: "${SEQ:=0}"
: "${N_PEERS:=1}"
: "${RATETIME:=20}"
: "${RATEWINDOW:=5000}"
: "${PATHCOUNT:=100000}"
: "${GROUPSIZE:=10}"
: "${MAXBURSTCOUNT:=80000}"
: "${REPEAT:=1}"
: "${DOCKER_NCPUS:=8}"

HOSTNAME=$(hostname)

set_docker_vars() {
  case "${HOSTNAME}" in
  "alef01" | "jupiter" | "saturn" | "statler" )
    DOCKER_MEMORY="96g"
    DOCKER_NCPUS="16"
    ;;
  "zeus" )
    DOCKER_MEMORY="256g"
    DOCKER_NCPUS="32"
    ;;
  "dell" | "xps9320" | "noble")
    DOCKER_MEMORY="16g"
    DOCKER_NCPUS="4"
    ;;

  *)
    echo "unknown host: ${HOSTNAME}"
    exit 1
    ;;
  esac
  DOCKER_CONSTRAINTS="--cpus=$DOCKER_NCPUS --memory=$DOCKER_MEMORY"
  DOCKER_LOGTEXT="DOCKER_NCPUS=$DOCKER_NCPUS DOCKER_MEMORY=$DOCKER_MEMORY"
  DOCKER_RUN_BASE="docker run --rm --cap-add NET_ADMIN --cap-add SYS_ADMIN --network host"
  DOCKER_RUN="$DOCKER_RUN_BASE -d $DOCKER_CONSTRAINTS"
}

# PERFWRAPPER="LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libc_malloc_debug.so.0"
# PERFWRAPPER="perf record -g -F 99"
PERFWRAPPER=""

if [[ -z "$target" ]]; then
  PROG=${1-"bird2"}
else
  PROG="$target"
fi

if [[ "$PROG" == "libvirt" ]]; then
  VMNAME=$(virsh list --name --state-running)
  VMCOUNT=$(echo "$VMNAME" | wc -w)
  if [[ $VMCOUNT -eq 1 ]]; then
    PROG=$VMNAME
  elif [[ $VMCOUNT -eq 0 ]]; then
    echo "no libvirt VM running"
    exit 1
  else
    echo "more than one libvirt VM running"
    exit 1
  fi
fi

KVARS="TIMEOUT RATEBLOCKSIZE MAXBLOCKINGFACTOR REPEAT IDLETHR SEEDPREFIX CANARYSEED SEEDPREFIXLEN GROUPSIZE RATEWINDOW PREFIXCOUNT PATHCOUNT MAXBURSTCOUNT RATECOUNT RATETIMELIMIT PEERMAXRETRIES REPEATDELAY TCPPORT SHOWRATE HOLDTIME LOGFILE LOGPATH LOGTEXT SENDFILENAME MODE"

DOCKER_TARGETS="bird1 bird2 bird3 relay gobgp hbgp frr bgpd"
CONTAINERS="kakapo $DOCKER_TARGETS"
DEFAULTS="LOGTEXT=\"$PROG TAG=$TAG SEQ=$SEQ SPEC=$SPEC N_PEERS=$N_PEERS PSU_STATUS=$PSU_STATUS $DOCKER_LOGTEXT\" TIMEOUT=20 REPEAT=$REPEAT PATHCOUNT=$PATHCOUNT"

# todo merge with defaults?
KAKAPO_ENV="$DEFAULTS GROUPSIZE=$GROUPSIZE MAXBURSTCOUNT=$MAXBURSTCOUNT RATEWINDOW=$RATEWINDOW RATETIMELIMIT=$RATETIME MODE=$MODE"

SCRIPT_DIR=$(realpath $(dirname "$0"))
CONFIG="$SCRIPT_DIR/conf/$PROG.conf"
if [[ -f "$CONFIG" ]]; then
  :
else
  echo "can't run, $CONFIG not exists"
fi
TESTING_DIR=$(realpath "$SCRIPT_DIR/..")
KAKAPO_DIR=$(realpath "$TESTING_DIR/..")

for n in $(seq 20 $((N_PEERS + 19))); do
  PEERS="$PEERS 172.18.0.13,172.18.0.${n},64504"
done

set_docker_vars

run_kakapo() {
  eval "${KAKAPO_ENV}"
  ENVSTR=""
  for envvar in $KVARS; do
    ENVSTR="$ENVSTR --env $envvar"
    if [[ ! -z "${!envvar}" ]]; then
      echo "$envvar is ${!envvar}"
      export ${envvar}
    fi
  done
  CURDIR="$(realpath $PWD)"
  touch $CURDIR/kakapo.json
  MAP_KAKAPO_JSON="--volume $CURDIR/kakapo.json:/kakapo.json"
  KAKAPO_COMMAND="$DOCKER_RUN_BASE -it $ENVSTR $MAP_KAKAPO_JSON --name kakapo kakapo 172.18.0.13,172.18.0.19,64505 $PEERS"
  # echo "kakapo command is $KAKAPO_COMMAND"
  eval "$KAKAPO_COMMAND"
}

docker_clean() {
  docker kill $CONTAINERS &>/dev/null || :
  docker rm $CONTAINERS &>/dev/null || :
}

docker_stop_wait() {
  docker kill $PROG &>/dev/null || :
  docker rm $PROG &>/dev/null || :
  while docker container inspect --format '{{.ID}}' $PROG &>/dev/null; do
    echo -n '.'
    sleep 1.0
  done
  echo "killed $PROG"
}

set_command() {

  local COMMAND
  case $PROG in

  bgpd | bird1 | bird2 | bird3 | gobgp | hbgp) COMMAND="$DOCKER_RUN --volume ${CONFIG}:/config/bgpd.conf --name $PROG $PROG" ;;

  frr) COMMAND="$DOCKER_RUN --env BGPLISTENADDR=172.18.0.13 --volume ${CONFIG}:/config/bgpd.conf --name $PROG $PROG" ;;

  relay) COMMAND="$DOCKER_RUN --name relay relay 172.18.0.13 172.18.0.19" ;;

  libvirt) COMMAND="echo \"check VM started...\"" ;;

  esac

  if [[ -z "${COMMAND}" ]]; then
    echo "unknown daemon: $PROG"
    exit 1
  fi

  echo ${COMMAND}
}

$TESTING_DIR/netns.sh del &>/dev/null || :

if [[ "$PROG" == "libvirt" ]]; then
  $SCRIPT_DIR/add_loopbacks.sh del lo $((N_PEERS + 1)) &>/dev/null
  $SCRIPT_DIR/add_loopbacks.sh add virbr1 $((N_PEERS + 1)) &>/dev/null
else
  $SCRIPT_DIR/add_loopbacks.sh del virbr1 $((N_PEERS + 1)) &>/dev/null
  $SCRIPT_DIR/add_loopbacks.sh add lo $((N_PEERS + 1)) &>/dev/null
fi

docker_clean
CMND=$(set_command $PROG)
bash -c "${CMND}"
run_kakapo
docker_stop_wait $PROG
docker_clean
