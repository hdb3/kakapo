base=/home/nic/vmx/vmx/images/junos-vmx-x86-64-18.2R1.9.qcow2

scriptd=`dirname "$0"`
echo "script is in $scriptd"
mkdir -p images

function base {
     vm=$1
     intnet="--network network=default2,model=virtio"
     extnet="--network network=default1,model=virtio"
     vmspec="--os-variant freebsd12.0 --memory 2048"
     # vmspec="--os-variant freebsd12.0 --graphics none --memory 2048"
     networks="$intnet"
     if (( $# < 2 )) ; then netn=1 ; else netn=$2 ; fi
     for i in $( seq 1 $netn ) 
         do networks="$networks $extnet"
         done
     virsh -q destroy $vm > /dev/null  || :
     virsh -q undefine $vm > /dev/null || :
     sudo rm -f images/$vm.qcow2
     cp $base images/$vm.qcow2
     virt-install --quiet --name $vm $vmspec  --noreboot --noautoconsole --import --disk images/$vm.qcow2 $networks
     virsh start --console $vm
}

base "base" 3
