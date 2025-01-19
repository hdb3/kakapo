
# single use script to define the tcpdump/tshark/wireshark environment for BGP testing
# note this will enable the access for the current user, but ONLY for subsequent login sessions

sudo groupadd pcap
sudo usermod -a -G pcap $USERNAME
for app in tcpdump dumpcap ; do
  this=$(sudo which $app)
  echo "enabling $this"
  sudo chgrp pcap $this
  sudo setcap cap_net_raw,cap_net_admin=eip $this
done
sudo setcap cap_net_raw,cap_net_admin=eip ./build-nets
