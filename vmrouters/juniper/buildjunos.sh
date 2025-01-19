#!/bin/bash -e
imagedir=$HOME/vmimages
base=${imagedir}/junos.qcow2
scriptd=$(realpath $(dirname "$0"))

PARENTDIR=$(realpath $scriptd/..)
SSHKEY="$PARENTDIR/sshkeys/junos.rsa"
JUNOSUSER="nic"
chmod og-rw $SSHKEY

jssh="ssh -l $JUNOSUSER -i $SSHKEY"

echo "491d74204f4971dc6c8f525b6151fc2e  $base" | md5sum -c - || exit 1

TARGET=${1:-"junos-1"}

function check_net {
  if ! virsh net-info $1 &>/dev/null; then
    echo "network $1 not defined"
  fi
}

function get_conf_file() {
  vm=$1
  conf_file="${scriptd}/junos/$vm.set"
  if [[ -f "$conf_file" ]]; then
    echo $conf_file
  else
    echo "no config file for target $vm ("${scriptd}/junos/$vm.set")" >&2
    echo ""
  fi
}

function get_junos_ip_address() {
  conf=$1
  addr=$(cat $conf | grep 'set interfaces vtnet0 unit 0 family inet address' | grep -Eo '[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*')
  # addr=$(sed -nE 's/set interfaces vtnet0 unit 0 family inet address ([.0-9]*)/\1/p' $conf)
  if [[ -z "$addr" ]]; then echo "no IP address found in  $conf" >&2; else
    echo "found IP address $addr in $conf" >&2
  fi
  echo $addr
}

function check_junos_ip_and_ssh() {
  host=$1
  if fping -q -r 50 $host; then echo "ping $host OK"; else
    echo "ping $host FAIL"
    exit 1
  fi

  if echo "show version" | $jssh $host | grep "Model: olive"; then echo "ssh $host OK"; else
    echo "ssh $host FAIL"
    exit 1
  fi
}

check_net "default1"
check_net "default2"

function build {
  vm=$1
  netn=$2
  shift 2

  conf_file="${scriptd}/junos/$vm.set"
  if [[ ! -f "$conf_file" ]]; then
    echo "no config file for target $vm ($conf_file)" >&2
    return 1
  fi

  image_file=${imagedir}/$vm.qcow2

  intnet="--network network=default2,model=virtio"
  extnet="--network network=default1,model=virtio"
  vmspec="--os-variant freebsd12.0 --memory 4096"
  networks="$ctlnet $intnet"
  for _ in $(seq 1 $netn); do
    networks="$networks $extnet"
  done
  virsh -q destroy $vm &>/dev/null || :
  virsh -q undefine $vm &>/dev/null || :
  sudo rm -f ${image_file}
  cp $base $image_file
  sudo chown libvirt-qemu:kvm $image_file
  virt-install --quiet --noreboot --noautoconsole $vmspec --name $vm --import --disk ${imagedir}/$vm.qcow2 $networks
  $scriptd/junos.ex $vm ${scriptd}/junos/common.set $* $conf_file

  addr=$(get_junos_ip_address $conf_file)
  if [[ -z "$addr" ]]; then echo "could not check network access for $vm"; else check_junos_ip_and_ssh $addr; fi
  virsh destroy $vm &>/dev/null
  echo "build complete for $vm"
}

if [[ ! "$TARGET" == "all" ]]; then
  build $TARGET 3
else
  # for f in junos/be*set junos/ap*set; do
  for f in junos/ap*set ; do
    ff="${f##junos/}"
    vm="${ff%.set}"
    echo "building vm $vm (from $f)"
    build $vm 3 "${scriptd}/junos/controller.set"
  done
fi
