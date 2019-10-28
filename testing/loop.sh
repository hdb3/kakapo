
#
## script control

: ${PLATFORMS:="BIRD BIRD2 FRR OPENBGPD HBGP RELAY"}
: ${RANGE:="5 10 20 30 40"}
: ${LOOPCOUNT:=1}
: ${SRC:="/home/nic/src"}

#
# some default locations - override in the environment if needed
# in particular, CONFSUBDIR allows to use other configuration files

: ${CONFSUBDIR:="simple"}
: ${BINDIR:="$SRC/kagu/build"}
: ${KAKAPODIR:="$SRC/kakapo/"}
: ${CONFDIR:="$KAKAPODIR/testing"}
#
FULLCONFDIR="${CONFDIR}/$CONFSUBDIR"

BIRD="$BINDIR/bird -d -c $FULLCONFDIR/bird.conf"
BIRD2="$BINDIR/bird2 -d -c $FULLCONFDIR/bird2.conf"
OPENBGPD="$BINDIR/bgpd -d -f $FULLCONFDIR/bgpd.conf"
FRR="$BINDIR/frr -S -l 172.18.0.13 -n --log stdout -f  $FULLCONFDIR/frr.conf"
HBGP="$BINDIR/hbgp $FULLCONFDIR/bgp.conf"
RELAY="$KAKAPODIR/relay/relay2 172.18.0.13 172.18.0.19"

## these are kakapo control variables - must be exported for kakapo process to inherit them

# kakapo variables
: ${REPEAT:=5}
: ${TIMEOUT:=30}
: ${TABLESIZE:=160000}
: ${MAXBURSTCOUNT:=50000}
: ${GROUPSIZE:=5}
: ${RATECOUNT:=1000000}
: ${REPEATDELAY:=1}
: ${WINDOW:=5000}
: ${MODE:="PAM"}
KVARS="MAXBURSTCOUNT GROUPSIZE RATECOUNT REPEATDELAY WINDOW TIMEOUT REPEAT MODE TABLESIZE"
KENV=""
for v in $KVARS
  do
    KENV="${KENV}$v=${!v} "
  done

KAKAPO="$KAKAPODIR/core/kakapo"

SCRIPTDIR=$( dirname "${BASH_SOURCE[0]}" )
VARS=$DIR/vars

#

echo "configs in $FULLCONFDIR, binaries in $BINDIR, relay2 in $( dirname $RELAY )"
echo "kakapo is $KAKAPO"

for n in `seq 1 $LOOPCOUNT` 
  do
    for PLATFORM in $PLATFORMS
      do
        BIN=${!PLATFORM}
        for m in $RANGE
          do
          KPEERCONFIG=peers.${m}
          KPEERS="$(<$SCRIPTDIR/$KPEERCONFIG)"
          LOGTEXT="$PLATFORM/$CONFSUBDIR/$KPEERCONFIG"
          KENV+=" LOGTEXT=$LOGTEXT"
          TESTBIN="$KENV $KAKAPO $KPEERS"
          # TESTBIN="bash -xe $CONFDIR/${m}peers.sh"
          echo "run: $n/$m"
          echo "config: $KPEERCONFIG"
          echo "platform: $PLATFORM"
          echo "binary: $BIN"
          echo "testbin: $TESTBIN"
          echo "ip netns exec target $BIN & PID=\$!"
          if [ -z $DRYRUN ]
          then
            ip netns exec target $BIN & PID=$!
            sleep 5 
            eval "$KENV ip netns exec kakapo $KAKAPO $KPEERS"
            kill $PID 
            wait
          else
            echo "ip netns exec target $BIN"
            echo "$KENV ip netns exec kakapo $KAKAPO $KPEERS"
          fi
        done
    done
  done
