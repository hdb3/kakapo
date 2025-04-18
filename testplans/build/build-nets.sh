#!/bin/bash -ex

# single use script to define the libvirt network environment for BGP testing

scriptd=$(realpath $(dirname "$0"))

if [[ "$1" == "create" ]]; then

  for net in default1 default2; do
    virsh net-define $scriptd/$net.xml
    virsh net-autostart $net
    virsh net-start $net
  done
  virsh net-list

elif [[ "$1" == "destroy" ]]; then

  for net in default1 default2; do
    virsh net-destroy $net
    virsh net-undefine $net
  done

else
  echo "please give command create or destroy"
fi
