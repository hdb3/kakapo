#!/bin/bash -e
# bootstrap is intended to be used in a clean environment after cloning the test repo
# an important part of bootstrap is installing the VM resources required
#
# this script is written and tested to be run as a bash script and exit early on failure
#
# task 1
# locate the junos and ios images and make a local symbolic link to them for use by later scripts
#
# the size and checksum are specific to the versions of junos and vious available at the time of writing
#

if ! virsh capabilities | grep -q qemu
then
  echo "libvirt cannot use qemu"
else
  echo "libvirt can use qemu"
  IMAGEDIR=$HOME/vmimages
  mkdir -p $IMAGEDIR
  vios=$IMAGEDIR/vios.vmdk
  junos=$IMAGEDIR/junos.qcow2

  echo "scanning for cisco VIOS image"
  if ! find ~ -name vios-adventerprisek9-m.vmdk.SPA.157-3.M3 -size 134021120c -exec ln -s "{}" $vios \;
  then
    echo "cisco VIOS image not found"
  elif ! echo "37c148ffa14a82f418a6e9c2b049fafe $vios" | md5sum -c
  then
    echo "cisco VIOS image failed checksum check"
  else
    echo "success - cisco VIOS image found"
  fi

  echo "scanning for juniper vMX image"

  if !   find ~ -name junos-vmx-x86-64-18.2R1.9.qcow2 -size 1334771712c -exec ln -s "{}" $junos \;
  then
    echo "juniper vMX image not found"
  elif !   echo "491d74204f4971dc6c8f525b6151fc2e $junos" | md5sum -c
  then
    echo "juniper vMX image failed checksum check"
  else
    echo "success - juniper vMX image found"
  fi

  #
  echo "successfully located ios and junos"

  # allow kvm/qemu/libvirt to access images in user directories
  chmod a+xr $HOME
  chmod a+xr $IMAGEDIR
fi
