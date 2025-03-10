#!/bin/bash -ex
imagedir=$HOME/vmimages
base=${imagedir}/junos.qcow2
scriptd=$(realpath $(dirname "$0"))
echo "491d74204f4971dc6c8f525b6151fc2e  $base" | md5sum -c - || exit 1
echo "script is in $scriptd"
TARGET=${1:-"external1"}

function check_net {
  if ! virsh net-info $1 &> /dev/null
  then
    echo "network $1 not defined"
  fi
}

check_net "default1"
check_net "default3"

function build {
    vm=$1
    shift 1
    intnet="--network network=default3,model=virtio"
    extnet="--network network=default1,model=virtio"
    vmspec="--os-variant freebsd12.0 --memory 4096"
    networks="$intnet  $extnet"
    virsh -q destroy $vm &> /dev/null  || :
    virsh -q undefine $vm &> /dev/null || :
    sudo rm -f ${imagedir}/$vm.qcow2
    cp $base ${imagedir}/$vm.qcow2
    sudo chown libvirt-qemu:kvm ${imagedir}/$vm.qcow2
    virt-install --quiet --noreboot --noautoconsole $vmspec --name $vm --import --disk ${imagedir}/$vm.qcow2 $networks
    $scriptd/junos.ex $vm "$@"
    virsh destroy $vm  &> /dev/null
}

if [[ -f "${scriptd}/junos/$TARGET.set" ]]
then
  build $TARGET "${scriptd}/junos/common.set" "${scriptd}/junos/$TARGET.set"
else
  echo "no config file for target $TARGET ("${scriptd}/junos/$TARGET.set")"
fi

exit 0
