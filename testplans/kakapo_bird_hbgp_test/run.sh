#!/bin/bash -e
# run a single bird instance with kakapo driving routes to it
# 'bird' is the DUT

REALDIR=$(realpath $(dirname "$0"))
if KAKAPO="$(which kakapo)"
then
  echo "kakapo is $KAKAPO"
else
  echo "not got kakapo"
  exit 1
fi

# kakapo usage is
# $KAKAPO $DUT,$KAKAPO_LISTEN,64505 $DUT,$KAKAPO_ORIGINATE_1 $DUT,$KAKAPO_ORIGINATE_2 $DUT,$KAKAPO_ORIGINATE_3 etc etc
# where $DUT is simply an IP address, and $KAKAPO_ORIGINATE_1/2/3/.. are <IP address>,<AS number> pairs
# e.g.
# KAKAPO="$(which kakapo)" || ( echo "kakapo not found" ; exit 1 )
DUT="127.0.1.1"
AS="65002"
IPADDRS=('127.0.1.2' '127.0.1.3' '127.0.1.4' '127.0.1.5')

LISTEN="$DUT,${IPADDRS[0]},$AS"
ORIGINATE=("$DUT,${IPADDRS[1]},$AS" "$DUT,${IPADDRS[2]},$AS" "$DUT,${IPADDRS[3]},$AS")
KAKAPOCMD="$KAKAPO $LISTEN ${ORIGINATE[0]} ${ORIGINATE[1]} ${ORIGINATE[2]}"
KAKAPOCMD1="MODE=SINGLEONLY REPEAT=1 $KAKAPO $LISTEN ${ORIGINATE[0]}"
echo "kakapo command is $KAKAPOCMD"
echo "kakapo1 command is $KAKAPOCMD1"

if BIRD="$(which bird)"
then
  echo "bird test"
  sudo killall -9 bird 2>/dev/null || :
  sleep 0.5
  BIRDC="${BIRD}c"
  BIRDCMD="$BIRD -c $REALDIR/bird.conf"
  echo "bird command is $BIRDCMD"
  sudo $BIRDCMD
  sleep 0.5
  if (env $KAKAPOCMD1); then
      echo "bird test - kakapo exited successfully"
  else
      echo "bird test - kakapo test failure"
  fi
  sudo $BIRDC down
else
  echo "bird not found"
fi

if HBGP="$(which hbgp)"
then
  echo "got hbgp"
  sudo bash -c "$HBGP $REALDIR/hbgp.conf > /var/run/hbgp.log &"
  sleep 0.5
  if (env $KAKAPOCMD1); then
      echo "hbgp test - kakapo exited successfully"
  else
      echo "hbgp test - kakapo test failure"
  fi
  sudo killall hbgp 2>/dev/null || :
else
  echo "hbgp not found"
  exit 1
fi





