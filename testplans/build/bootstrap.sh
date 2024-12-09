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
# ./ubuntu-dependencies.sh
virsh capabilities | grep qemu
IMAGEDIR=$HOME/vmimages
mkdir -p $IMAGEDIR
vios=$IMAGEDIR/vios.vmdk
junos=$IMAGEDIR/junos.qcow2
find ~ -name vios-adventerprisek9-m.vmdk.SPA.157-3.M3 -size 134021120c -exec ln -s "{}" $vios \;
echo "37c148ffa14a82f418a6e9c2b049fafe $vios" | md5sum -c
#
find ~ -name junos-vmx-x86-64-18.2R1.9.qcow2 -size 1334771712c -exec ln -s "{}" $junos \;
echo "491d74204f4971dc6c8f525b6151fc2e $junos" | md5sum -c
echo "successfully located ios and junos"

# allow kvm/qemu/libvirt to access images in user directories
chmod a+xr $HOME
chmod a+xr $IMAGEDIR
