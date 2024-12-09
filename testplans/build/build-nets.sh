#!/bin/bash 

# single use script to define the libvirt network environment for BGP testing

if [[ "$1" == "create" ]]; then

for net in default1 default2
  do virsh net-define $net.xml
     virsh net-autostart $net
     virsh net-start $net
  done
virsh net-list

elif [[ "$1" == "destroy" ]]; then

for net in default1 default2
  do virsh net-destroy $net
     virsh net-undefine $net
  done

else
  echo "please give command create or destroy"
fi
