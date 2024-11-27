#!/bin/bash -e
if [[ "$1" == "del" ]]; then
    cmd="del"
else
    cmd="add"
fi
for ip in 172.18.0.13 172.18.0.18 172.18.0.19 172.18.0.20; do
    sudo ip addr $cmd $ip/32 dev lo || echo "error address was $ip"
done
