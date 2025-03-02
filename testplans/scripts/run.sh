#!/bin/bash -ex

which nets >/dev/null || (
  echo "can't locate utility 'nets'"
  exit 1
)
which vm >/dev/null || (
  echo "can't locate utility 'vm'"
  exit 1
)
export DOCKER_HOST=127.0.0.1
docker ps >/dev/null || exit 1
echo "prerequisite check OK"

scriptd=$(realpath $(dirname "$0"))

function get_script {
  if [[ -f $1 ]]; then
    eval ${2}="$1"
    ln -srf "$1" ${scriptd}/${2}
    elif [[ -f ${scriptd}/${1} ]]; then
    eval ${2}="${scriptd}/${1}"
    ln -srf "${scriptd}/$1" ${scriptd}/${2}
    # elif [[ -f ${scriptd}/${2} ]] ; then link=$(realpath "${scriptd}/${2}") ; eval ${2}="${link}"
    elif [[ -f ${scriptd}/${2} ]]; then
    eval ${2}=$(realpath "${scriptd}/${2}")
  else
    echo "$1 is not found and no saved default, nothing to do"
    exit 1
  fi
  echo "using ${!2} as ${2}"
}

get_script "$1" test_script
get_script "$2" target_script
echo "will run test script $test_script with target $target_script"
source ${scriptd}/local.sh
source ${scriptd}/common.sh
source ${scriptd}/layouts.sh
tmux kill-session -t test >/dev/null 2>&1 || :
tmux wait-for test >/dev/null 2>&1 || :
killall wireshark || :
for image in bird2 frr gobgp bgpd; do DOCKER_HOST=127.0.0.1 docker kill $image 2>&1 >/dev/null || :; done
interface=lo
source ${target_script}
source ${test_script}
tmux kill-session -t test
