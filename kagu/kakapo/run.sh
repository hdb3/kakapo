# run.sh for kakapo
echo "/coredumps/%e.%t" > /proc/sys/kernel/core_pattern
echo "starting kakapo"
ip addr add 172.18.0.21/32 dev lo || echo "probably the address is already assigned"
ip addr add 172.18.0.22/32 dev lo || echo "probably the address is already assigned"
