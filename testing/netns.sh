#!/bin/bash -e
IFT=em3
IFK=em4
if [[ "$1" == "del" ]]; then
  set -x
  sudo ip netns del target || :
  sudo ip netns del kakapo || :
  sudo ip link delete $IFT || :
else
  set -x
  ip link add $IFT type veth peer name $IFK
  ip netns add target
  ip netns add kakapo
  ip link set $IFT netns target
  ip link set $IFK netns kakapo
  ip netns exec target ip li set up dev $IFT
  ip netns exec kakapo ip li set up dev $IFK
  ip netns exec target ip addr add 172.18.0.13/24 dev $IFT
  set +x
  for n in $(seq 19 70); do
    ip netns exec kakapo ip addr add 172.18.0.${n}/24 dev $IFK
    ip netns exec target fping -r 20 -u 172.18.0.${n}
  done
fi
