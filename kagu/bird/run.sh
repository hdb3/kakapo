# run.sh for bird
if [ "$1" = "sysinfo" ]
then
  echo "VERSION='`/usr/sbin/bird --version |& cat`'"
  /usr/bin/sysinfo
else
  echo "starting bird"
  # ip addr add 172.18.0.13/32 dev lo || echo "probably the address is already assigned"
  # until fping -S 172.18.0.13 172.18.0.21 172.18.0.22 ; do echo "retry" ; sleep 10 ; done
  # /bin/bash -il
  /usr/sbin/bird -d
fi
