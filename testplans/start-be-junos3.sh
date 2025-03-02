for vm in $(virsh list --name); do virsh destroy $vm; done
for vm in be-junos-3a be-junos-3b be-junos-3c; do virsh start $vm; done
virsh list
