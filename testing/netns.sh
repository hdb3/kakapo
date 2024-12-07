#!/bin/bash -e
IFT=enb3
IFK=en4b
if [[ "$1" == "del" ]]; then
  set -x
  sudo ip netns del target || :
  sudo ip netns del kakapo || :
else
  set -x
  ip netns add target
  ip netns add kakapo
  ip link set $IFT netns target
  ip link set $IFK netns kakapo
  ip netns exec target ip li set up dev $IFT
  ip netns exec kakapo ip li set up dev $IFK
  ip netns exec target ip addr add 172.18.0.13/24 dev $IFT
  for n in $(seq 19 22); do
    ip netns exec kakapo ip addr add 172.18.0.${n}/24 dev $IFK
    ip netns exec target fping -r 20 -u 172.18.0.${n}
  done
fi
