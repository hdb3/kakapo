#!/bin/bash -e
SCRIPTDIR=$(realpath $(dirname "$0"))
: ${BINDIR:="$SCRIPTDIR/bin"}
: ${BASEDIR:=$(realpath "$SCRIPTDIR/..")}

# test topology
#
# DUT receives from multiple internal peers and sends to a single external peer
#
# local address: 172.18.0.13
# external peer address: 172.18.0.19
# internal peers: 172.18.0.20...
#
LOCAL="172.18.0.13"
EXTERNAL="172.18.0.19"
#
## script control

# : ${PLATFORMS:="BIRD BIRD2 FRR OPENBGPD HBGP RELAY GOBGP"}
: ${PLATFORMS:="BIRD2 OPENBGPD FRR RELAY GOBGP"}
: ${RANGE:="01 05 10 15 20 25 30 35 40 45 50"}
: ${LOOPCOUNT:=1}

#
# some default locations - override in the environment if needed
# in particular, CONFSUBDIR allows to use other configuration files

: ${CONFSUBDIRS:="simple filters"}
: ${CONFDIR:="$SCRIPTDIR"}
#

BIRD_CMD="$BINDIR/bird/bird -d -c "
BIRD2_CMD="$BINDIR/bird2/bird -d -c "
OPENBGPD_CMD="$BINDIR/bgpd -d -f "
GOBGP_CMD="$BINDIR/gobgpd -f "
FRR_CMD="$BINDIR/frr -S -l $LOCAL -n --log stdout -f  "
HBGP_CMD="$BINDIR/hbgp "
RELAY_CMD="$BASEDIR/relay/relay2"
KAKAPO_CMD="$BASEDIR/bin/kakapo"

## these are kakapo control variables - must be exported for kakapo process to inherit them

# kakapo command line configuration variables
: ${REPEAT:=5}
: ${TIMEOUT:=30}
: ${TABLESIZE:=160000}
: ${MAXBURSTCOUNT:=50000}
: ${GROUPSIZE:=5}
: ${RATECOUNT:=100000}
: ${REPEATDELAY:=1}
: ${WINDOW:=5000}
: ${MODE:="PAM"}
KVARS="MAXBURSTCOUNT GROUPSIZE RATECOUNT REPEATDELAY WINDOW TIMEOUT REPEAT MODE TABLESIZE"
KENV=""
for v in $KVARS; do
  KENV="${KENV}$v=${!v} "
done

echo "configs in $CONFDIR, binaries in $BINDIR, relay2 in $(dirname $RELAY_CMD)"
echo "kakapo is $KAKAPO_CMD, platforms: $PLATFORMS"

for n in $(seq 1 $LOOPCOUNT); do
  for CONFSUBDIR in $CONFSUBDIRS; do
    FULLCONFDIR="${CONFDIR}/$CONFSUBDIR"

    BIRD="$BIRD_CMD $FULLCONFDIR/bird.conf"
    BIRD2="$BIRD2_CMD $FULLCONFDIR/bird2.conf"
    OPENBGPD="$OPENBGPD_CMD $FULLCONFDIR/bgpd.conf"
    GOBGP="$GOBGP_CMD $FULLCONFDIR/gobgpd.conf"
    FRR="$FRR_CMD $FULLCONFDIR/frr.conf"
    HBGP="$HBGP_CMD $FULLCONFDIR/bgp.conf"
    RELAY="$RELAY_CMD $LOCAL $EXTERNAL"
    for PLATFORM in $PLATFORMS; do
      BIN=${!PLATFORM}
      for m in $RANGE; do
        KPEERCONFIG=peers.${m}
        KPEERS="$(<$SCRIPTDIR/$KPEERCONFIG)"
        LOGTEXT="$PLATFORM/$CONFSUBDIR/$KPEERCONFIG"
        CMD_ENV="$KENV LOGTEXT=$LOGTEXT"
        echo "run: $n/$m"
        echo "config: $KPEERCONFIG"
        echo "platform: $PLATFORM"
        echo "binary: $BIN"
        echo "testbin: $TESTBIN"
        echo "ip netns exec target $BIN & PID=\$!"
        if [ -z $DRYRUN ]; then
          ip netns exec target $BIN &
          PID=$!
          sleep 5
          eval "ip netns exec kakapo bash -c \"$CMD_ENV $KAKAPO_CMD $KPEERS\""
          kill $PID
          wait
        else
          echo "ip netns exec target $BIN"
          echo "ip netns exec kakapo bash -c \"$CMD_ENV $KAKAPO_CMD $KPEERS\""
        fi
      done
    done
  done
done
