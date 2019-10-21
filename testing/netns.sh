ip netns add target
ip netns add kakapo
ip link set em1 netns target
ip link set em2 netns kakapo
ip netns exec target ip li set up dev em1
ip netns exec kakapo ip li set up dev em2

ip netns exec target ip addr add 172.18.0.13/24 dev em1
for n in `seq 19 60`
  do
    ip netns exec kakapo ip addr add 172.18.0.${n}/24 dev em2
    ip netns exec target fping -r 20 -u 172.18.0.${n}
  done
exit 0
ip netns del target
ip netns del kakapo
