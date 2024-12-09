# testplans #

Testplans

miscellaneous resources for testing and test orchestration of BGP functionality

## target systems ##

configurations and scripts support testing many BGP agents including hBGP, Cisco vIOS and Juniper vMX, bird, frr and exabgp

## 'sudo' escapes ##

Binding to low numbered TCP ports and interface packet capture require root privileges by default.
This is problematic for test scripts running from or using non-root build accounts.
On development machines a solution is to allow non-root users the privilege for these operations.
For packet capture the documented solutions for tcpdump and wireshark are suitable.
For binding low ports the solution used here is to change the Linux kernel sysctrl parameter 'ip_unprivileged_port_start'.
See 'priv.sh'.

## tmux automation ##

tmux enables multiple interactive application sessions to be monitored and interacted with in a single window.  tmux also allows pre-defined scripts to be applied via CLI input to multiple applications in a controlled sequence.
In future, application behaviour could also be observed and evaluated for conformance (perhaps a monitor mode of hBGP with regularly formatted output to a pipe or other distinct stream?).

The basic tmux framework starts multiple BGP speakers in separate panes, and then sends command sequences to apps as required by the test scenario.
This also allows virtualised router instances to be managed, although slow startup and the requirement to detect when a virtual router has come online are issues which are not yet addressed - perhaps a functional test of end-to-end routing would be best?
However, as long as the virtual routers are passive elements of an experiment there should be no need to repeatedly start and stop them as part of a test.
