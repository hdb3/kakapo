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
CONFDIR="~/src/kakapo/testing"
BINDIR="~/src/kagu/build"
BIRD="$BINDIR/bird-1.6.6/bird -d -c $CONFDIR/bird.conf.50"
BIRD2="$BINDIR/bird-2.0.4/bird -d -c $CONFDIR/bird2.conf.50"
OPENBGPD="$BINDIR/openbgpd-6.5p0/bgpd -d -f $CONFDIR/bgpd.conf.50"
FRR="$BINDIR/frr -S -l 172.18.0.13 -n --log stdout -f  $CONFDIR/frr.conf.50"
HBGP="$BINDIR/hbgp $CONFDIR/bgp.conf"
RELAY="~/src/kakapo/relay/relay2 172.18.0.13 172.18.0.19"
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
