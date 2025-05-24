# sudo="sudo"
newuser="nic"
$sudo userdel $newuser
$sudo rm -rf /home/$newuser/
$sudo adduser --disabled-password --gecos "" $newuser
$sudo usermod -a -G sudo $newuser
$sudo cp -r ~/.ssh /home/$newuser
$sudo chown -R $newuser:$newuser /home/$newuser/.ssh
# $sudo bash -c 'echo " $newuser ALL=(ALL:ALL) NOPASSWORD: ALL"' >> /etc/sudoers 
