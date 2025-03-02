for vm in $(virsh list --name); do virsh destroy $vm; done
for vm in ap-junos-3a ap-junos-3b ap-junos-3c; do virsh start $vm; done
virsh list
