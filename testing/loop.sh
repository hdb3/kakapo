#
PLATFORMS=${PLATFORMS:-"BIRD BIRD2 FRR OPENBGPD HBGP RELAY"}
RANGE=${RANGE:-"5 10 20 30 40"}
REPEAT=${REPEAT:-5}
TIMEOUT=${TIMEOUT:-30}
TABLESIZE=${TABLESIZE:-160000}

# : ${var1:=foo}
: ${MAXBURSTCOUNT:=50000}
: ${GROUPSIZE:=5}
#
#

## these are kakapo control variable - must be exported for kakapo process to inherit them
export RATECOUNT=1000000
export WINDOW=5000
export MODE=PAM
export REPEAT
export REPEATDELAY=1
export TIMEOUT
export TABLESIZE
export MODE=PAM
export GROUPSIZE
export MAXBURSTCOUNT

#
LOOPCOUNT=${LOOPCOUNT:-1}
#
# some default locations - overiide in the environmanet if needed
# in particular, CONFSUBDIR allows to use other configuration files
CONFDIR=${CONFDIR=:-"$HOME/src/kakapo/testing"}
CONFSUBDIR=${CONFSUBDIR:-"simple"}
BINDIR=${BINDIR:-"$HOME/src/kagu/build"}
KAKAPODIR=${KAKAPODIR:-"$HOME/src/kakapo/"}
#
FULLCONFDIR="${CONFDIR}/$CONFSUBDIR"
echo "configs in $FULLCONFDIR, binaries in $BINDIR, relay2 in $RELAYDIR"
# CONFDIR="~/src/kakapo/testing"
# BINDIR="~/src/kagu/build"
BIRD="$BINDIR/bird -d -c $FULLCONFDIR/bird.conf"
BIRD2="$BINDIR/bird2 -d -c $FULLCONFDIR/bird2.conf"
OPENBGPD="$BINDIR/bgpd -d -f $FULLCONFDIR/bgpd.conf"
FRR="$BINDIR/frr -S -l 172.18.0.13 -n --log stdout -f  $FULLCONFDIR/frr.conf"
HBGP="$BINDIR/hbgp $FULLCONFDIR/bgp.conf"
RELAY="$KAKAPODIR/relay/relay2 172.18.0.13 172.18.0.19"
export KAKAPO="$KAKAPODIR/core/kakapo"
for n in `seq 1 $LOOPCOUNT` 
  do
    for PLATFORM in $PLATFORMS
      do
        for m in $RANGE
          do
          CONFIG=${m}peers.sh
          export LOGTEXT="$PLATFORM/$CONFSUBDIR/$CONFIG"
          BIN=${!PLATFORM}
          TESTBIN="bash -xe $CONFDIR/${m}peers.sh"
          echo "run: $n/$m"
          echo "config: $CONFIG"
          echo "platform: $PLATFORM"
          echo "binary: $BIN"
          echo "testbin: $TESTBIN"
          echo "ip netns exec target $BIN & PID=\$!"
          if [ -z $DRYRUN ]
          then
            ip netns exec target $BIN & PID=$!
            sleep 5 
            ip netns exec kakapo $TESTBIN
            kill $PID 
            wait
          else
            echo "ip netns exec target $BIN"
            echo "ip netns exec kakapo $TESTBIN" 
          fi
        done
    done
  done
