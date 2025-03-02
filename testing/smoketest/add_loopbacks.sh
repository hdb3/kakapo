#!/bin/bash -e

if [[ "$1" == "del" ]]; then
  cmd="del"
else
  cmd="add"
fi

IFACE=${2:-"lo"}

N_ADDRESSES=${3:-"3"}


if [[ "$IFACE" == "lo" ]]; then
  ADDRESSES="172.18.0.13 "
  MASK="32"
else
  MASK="24"
fi

for n in $(seq 19 $((N_ADDRESSES + 18)))
do
  ADDRESSES="$ADDRESSES 172.18.0.${n}"
done


# TDOD - autodetect what interface owns these addresses, so can remove them without being told

for ip in  $ADDRESSES; do
  sudo ip addr $cmd $ip/$MASK dev $IFACE || echo "error address was $ip/$MASK"
done
