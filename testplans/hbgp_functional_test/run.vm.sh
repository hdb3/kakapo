#!/bin/bash -e
CONF=bird.conf.vm
DIR=$(dirname "$0")
REALDIR=`realpath $DIR`
CONFDIR="$REALDIR/$CONF"
BIRD="`which bird`"
BIRDC="${BIRD}c"
BIRDC3="sudo $BIRDC -s /var/run/bird3"
TIMEOUT=600

start_birds() {
  sudo killall bird 2> /dev/null ||:
  sudo killall -9 bird 2> /dev/null ||:
  sleep 0.5
  sudo $BIRD -D /var/run/bird1.log -c $CONFDIR/as65001.conf -s /var/run/bird1
  sudo $BIRD -D /var/run/bird3.log -c $CONFDIR/as65003.conf -s /var/run/bird3
  echo "birds started"
}

stop_birds() {
    sudo $BIRDC -s /var/run/bird1 down &> /dev/null
    sudo $BIRDC -s /var/run/bird3 down &> /dev/null
}

check_route() {
  ROUTES=`$BIRDC3 show route`
  if [[ $ROUTES =~ "7.7.7.0/24" ]]
  then
    echo "success!"
    echo -n "bird(3) says: "
    sudo $BIRDC -s /var/run/bird3 show route | grep "7.7.7.0/24"
    echo ""
    return 0
  else
    return 1
  fi
}

loop_check_route() {
  stop_at=$((`date +%s` + $TIMEOUT))
  while [[ `date +%s` -lt $stop_at ]]
  do
    if check_route
    then
      return 0
    else
      sleep 0.5
      echo -n '.'
    fi
  done
  return 1
}


start_birds
sleep 1.0
if loop_check_route ; then
  stop_birds
  exit 0
else
  echo "fail!"
  sudo $BIRDC -s /var/run/bird3 show route
  echo ""
  stop_birds
  exit 1
fi
