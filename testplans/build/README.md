# scripts #

the scripts buildvios and buildjunos build and configure instances of Cisco and Juniper routers using configurations located in the sub-directories ios and junos respectively.

The scripts rely on the presence of a functioning libvirt environment and the virsh and virt-install command line tools.
Additionally, the additional virtual networks defined in local XML files must be defined in the libvirt environment.
This can be done using the one-time script build-nets.sh.
Finally, the junos and ios images are required.  The locations for these is hard-coded at the top of the build scripts.
