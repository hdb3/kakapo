#!/bin/bash -e
scriptd=$(realpath $(dirname "$0"))

alias cissh="ssh -l cisco -i $scriptd/sshkeys/vm.key -oKexAlgorithms=diffie-hellman-group14-sha1"

function check_net {
  if ! virsh net-info $1 &>/dev/null; then
    echo "network $1 not defined"
    exit 1
  fi
}

function build {
  vm=$1
  intnet="--network network=default2,model=e1000"
  extnet="--network network=default1,model=e1000"
  vmspec="--os-variant freebsd12.0 --graphics none --memory 2048"
  networks="$intnet"
  if (($# < 2)); then netn=1; else netn=$2; fi
  for i in $(seq 1 $netn); do
    networks="$networks $extnet"
  done
  virsh -q destroy $vm &>/dev/null || :
  virsh -q undefine $vm &>/dev/null || :
  sudo rm -f ${imagedir}/$vm.qcow2
  qemu-img convert -f vmdk -O qcow2 $vmdk ${imagedir}/$vm.qcow2
  sudo chown libvirt-qemu:kvm ${imagedir}/$vm.qcow2
  virt-install --quiet --noreboot --noautoconsole $vmspec --name $vm --import --disk ${imagedir}/$vm.qcow2 $networks
  $scriptd/vios.ex $vm $scriptd/ios/$vm.startup.cfg
  # # following depends on the VM acquiring an address via DHCP...
  # virsh -q start $vm &>/dev/null || :
  # fping -q -r 100 $vm
  # cissh $vm <$scriptd/ios/nobanner.cfg
  # virsh destroy $vm &>/dev/null || :
}

imagedir=$HOME/vmimages
vmdk=${imagedir}/vios.vmdk

if ! echo "37c148ffa14a82f418a6e9c2b049fafe $vmdk" | md5sum -c -; then
  echo "image file $base failed checksum"
elif check_net "default1" && check_net "default2"; then
  TARGET=${1:-"base"}
  if [[ -f "${scriptd}/ios/$TARGET.startup.cfg" ]]; then
    build $TARGET 3
  else
    echo "no config file for target $TARGET ("${scriptd}/ios/$TARGET.startup.cfg")"
  fi

  echo "VIOS build complete"
  exit 0
  # build complete set is optional

  build ios-1 3
  build ap-ios-1 3
  for vm in ap-ios-3a ap-ios-3b ap-ios-3c; do
    build $vm 1
  done

  for vm in be-ios-3a be-ios-3b be-ios-3c; do
    build $vm 1
  done
fi
