#!/bin/bash -e
DIR=$(dirname "$0")
REALDIR=`realpath $DIR`
BIRD="`which bird`"
BIRDC="${BIRD}c"
HBGP="`which hbgp`"
sudo killall bird hbgp 2> /dev/null ||:
sudo killall -9 bird hbgp 2> /dev/null ||:
echo "using $BIRD and $HBGP"
sleep 0.5
sudo $BIRD -D /var/run/bird1.log -c $REALDIR/bird.conf/as65001.conf -s /var/run/bird1
sudo $BIRD -D /var/run/bird3.log -c $REALDIR/bird.conf/as65003.conf -s /var/run/bird3

if [[ ${1^^} =~ "CONTROL" ]]
then
  echo "CONTROL CASE (3 BIRDs)"
  echo "sudo $BIRD -D /var/run/bird2.log -c $REALDIR/bird.conf/as65002.conf -s /var/run/bird2"
  sudo $BIRD -D /var/run/bird2.log -c $REALDIR/bird.conf/as65002.conf -s /var/run/bird2
else
  echo "DEFAULT CASE (2 BIRDs, 1 hbgp)"
  echo "sudo $HBGP $REALDIR/hbgp.conf > /var/run/hbgp.log &"
  sudo bash -c "$HBGP $REALDIR/hbgp.conf > /var/run/hbgp.log &"
fi


BIRDC3="sudo $BIRDC -s /var/run/bird3"
check_route() {
  ROUTES=`$BIRDC3 show route`
  if [[ $ROUTES =~ "7.7.7.0/24" ]]
  then
    return 0
  else
    return 1
  fi
}

sleep 1.0
stop_at=$((`date +%s` + 10))
while [[ `date +%s` -lt $stop_at ]]
do
  if check_route
  then
    echo "success!"
    sudo $BIRDC -s /var/run/bird3 show route
    exit 0
  else
    sleep 0.5
    echo -n '.'
  fi
done
echo ""
echo "fail!"
sudo $BIRDC -s /var/run/bird3 show route
exit 1
