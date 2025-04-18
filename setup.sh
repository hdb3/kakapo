for group in libvirt docker ; do
	sudo groupadd $group
	sudo usermod -a -G $group $USERNAME
done
sudo apt install -y make

