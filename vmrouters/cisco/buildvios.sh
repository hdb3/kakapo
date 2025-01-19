#!/bin/bash -e
scriptd=$(realpath $(dirname "$0"))
PARENTDIR=$(realpath $scriptd/..)
IOSSSHOPTS="-oKexAlgorithms=diffie-hellman-group14-sha1 -oHostKeyAlgorithms=ssh-rsa -oPubkeyAcceptedKeyTypes=ssh-rsa"
SSHKEY="$PARENTDIR/sshkeys/vm.key"
IOSUSER="cisco"
chmod og-rw $SSHKEY

cissh="ssh -l $IOSUSER -i $SSHKEY $IOSSSHOPTS"

imagedir=$HOME/vmimages
vmdk=${imagedir}/vios.vmdk
echo "37c148ffa14a82f418a6e9c2b049fafe $vmdk" | md5sum -c - >/dev/null || exit 1

function check_ios_ip_and_ssh {
  vm=$1
  # ping $vm
  # expect ~30 seconds for IOS to reboot
  if fping -q -r 50 $vm; then echo "ping $vm OK"; else
    echo "ping $vm FAIL"
    exit 1
  fi
  # TDOD could verify the reply to this version request..., e.g. some fragment of:
  #   -- KVIOS#show version
  #   -- Cisco IOS Software, IOSv Software (VIOS-ADVENTERPRISEK9-M), Version 15.7(3)M3, RELEASE SOFTWARE (fc2)
  if echo "show version" | $cissh $vm | grep "Cisco IOS Software, IOSv Software"; then echo "ssh $vm OK"; else
    echo "ssh $vm FAIL"
    exit 1
  fi
}

function check_net {
  if ! virsh net-info $1 &>/dev/null; then
    echo "network $1 not defined"
    exit 1
  fi
}

check_net "default1"
check_net "default2"

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
  virsh -q start $vm &>/dev/null || :
  check_ios_ip_and_ssh $vm
  if $cissh $vm <$scriptd/ios/nobanner.cfg|grep '\[OK\]' ; then echo "ssh config OK" ; else echo "ssh config FAIL" ; fi
}

TARGET=${1:-"base"}
if [[ -f "${scriptd}/ios/$TARGET.startup.cfg" ]]; then
  build $TARGET 3
else
  echo "no config file for target $TARGET ("${scriptd}/ios/$TARGET.startup.cfg")"
fi

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
