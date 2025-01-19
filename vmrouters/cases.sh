#!/bin/bash -e

t0="0.5" t1="1.5" t2="1.0"
addr="10.42.42.42"
set_off() {
	sudo ip address delete $addr/32 dev lo &>/dev/null || :
	sleep 0.1
}

set_on() {
	sudo ip address add $addr/32 dev lo &>/dev/null || :
	fping -q $addr
}

start() {
	./breaktest --target $addr &
}

case0() {
	set_off
	start
	sleep $t0
	set_on
	wait
	echo "case 0 done"
}

case1() {
	set_off
	start
	sleep $t0
	set_on
	sleep $t1
	set_off
	wait
	echo "case 1 done"
}

case2() {
	set_off
	start
	sleep $t0
	set_on
	sleep $t1
	set_off
	sleep $t2
	set_on
	wait
	echo "case 2 done"
}

case3() {
	set_off
	start
	sleep $t0
	set_on
	sleep $t1
	set_off
	sleep $t2
	set_on
	sleep $t0
	set_off
	wait
	echo "case 3 done"
}

if [[ "$1" == "0" ]]; then case0; fi
if [[ "$1" == "1" ]]; then case1; fi
if [[ "$1" == "2" ]]; then case2; fi
if [[ "$1" == "3" ]]; then case3; fi
if [[ "$1" == "" ]]; then
	case0
	case1
	case2
	case3
fi

wait
