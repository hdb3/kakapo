#!/bin/bash
for vm in br1 br3a br3b br3c ; do virsh destroy $vm ; done
for vm in br3a br3b br3c ; do virsh start $vm ; done
