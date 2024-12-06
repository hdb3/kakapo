#!/bin/bash -e

if [[ "$1" == "del" ]]; then
    cmd="del"
else
    cmd="add"
fi

IFACE=${2:-"lo"}

ADDRESSES="172.18.0.18 172.18.0.19 172.18.0.20"

if [[ "$IFACE" == "lo" ]]; then
  ADDRESSES="172.18.0.13 $ADDRESSES"
  MASK="32"
else
  MASK="24"
fi

# TDOD - autodetect what interface owns these addresses, so can remove them without being told

for ip in  $ADDRESSES; do
    sudo ip addr $cmd $ip/$MASK dev $IFACE || echo "error address was $ip/$MASK"
done

