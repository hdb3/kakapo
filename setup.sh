sudo apt update
sudo DEBIAN_FRONTEND=noninteractive apt -yy full-upgrade
sudo DEBIAN_FRONTEND=noninteractive apt install -yy make linux-generic-hwe-$(lsb_release -rs)

for group in libvirt docker ; do
	sudo groupadd $group
	sudo usermod -a -G $group $USER
done
scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -r nic@172.16.102.32:vrouter_images .
ssh-keyscan github.com >> ~/.ssh/known_hosts
git clone git@github.com:hdb3/kakapo.git -b dev || echo "kakapo already exists?"
