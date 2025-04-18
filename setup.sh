sudo apt update
sudo apt -y full-upgrade
for group in libvirt docker ; do
	sudo groupadd $group
	sudo usermod -a -G $group $USER
done
scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -r 172.16.102.32:vrouter_images .
sudo apt install -y make
newgrp docker
newgrp libvirt


