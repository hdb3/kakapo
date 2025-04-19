sudo apt update
sudo apt -y full-upgrade
for group in libvirt docker ; do
	sudo groupadd $group
	sudo usermod -a -G $group $USER
done
scp -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -r 172.16.102.32:vrouter_images .
ssh-keyscan github.com >> ~/.ssh/known_hosts
git clone git@github.com:hdb3/kakapo.git -b dev
sudo apt install -y make
# sudo systemctl reboot
