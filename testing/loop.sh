#
PLATFORMS="BIRD BIRD2 FRR HBGP RELAY"
#PLATFORMS="BIRD BIRD2 FRR OPENBGPD HBGP RELAY"
RANGE="5 10 20 30 40"
REPEAT=20
#
#
RATECOUNT=1000000
WINDOW=5000
MODE=PAM
REPEAT=5
REPEATDELAY=1
TIMEOUT=30
TABLESIZE=160000
MODE=PAM
GROUPSIZE=5
MAXBURSTCOUNT=50000
#
#RANGE="5"
#REPEAT=2
#
BIRD="/home/nic/src/kagu/bird/bird-1.6.6/bird -d -c /home/nic/src/kagu/bird/bird.conf.50"
BIRD2="/home/nic/src/kagu/bird2/bird-2.0.4/bird -d -c /home/nic/src/kagu/bird2/bird.conf.50"
OPENBGPD="/home/nic/src/kagu/bgpd/openbgpd-6.5p0/bgpd -d -f /home/nic/src/kagu/bgpd/bgpd.conf.50"
FRR="/usr/lib/frr/bgpd -S -l 172.18.0.13 -n --log stdout -f  /home/nic/src/kagu/frr/bgpd.conf.50"
HBGP="/home/nic/src/kagu/hbgp/hbgp /home/nic/src/kakapo-newrelay/relay/bgp.conf"
RELAY="/home/nic/src/kakapo-newrelay/relay/relay2 172.18.0.13 172.18.0.19"
for n in `seq 1 $REPEAT` 
do
for PLATFORM in $PLATFORMS
  do
  for m in $RANGE
    do
      # for n in `seq 1 $REPEAT` 
      #   do
          CONFIG=${m}peers.sh
          LOGTEXT="$PLATFORM/$CONFIG"
          BIN=${!PLATFORM}
          TESTBIN="bash -xe /home/nic/src/kakapo-newcore/testing/${m}peers.sh"
          echo "run: $REPEAT config: $CONFIG platform: $PLATFORM binary: $BIN testbin: $TESTBIN"
          echo "ip netns exec target $BIN & PID=\$!"
          ip netns exec target $BIN & PID=$!
          sleep 5 
          echo "ip netns exec kakapo $TESTBIN" 
          ip netns exec kakapo $TESTBIN
          kill $PID 
          wait
        done
    done
  done
