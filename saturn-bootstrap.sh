export VM=dhp@jupiter
scp setup.sh $VM:/tmp/setup.sh
ssh -Att $VM source /tmp/setup.sh
ssh -Att $VM 'sudo systemctl reboot'
sleep 10.0
while ! ssh -q -o ConnectionAttempts=1 -o ConnectTimeout=5 -o StrictHostKeyChecking=no -Att $VM 'pwd>/dev/null'; do
    echo -n '.'
done
echo "reconnected to $VM"
ssh -Att $VM 'cd kakapo && make'

# sudo userdel dhp
# sudo rm -rf /home/dhp/
# sudo adduser --disabled-password --gecos "" dhp
# sudo usermod -a -G sudo dhp
# sudo cp -r ~/.ssh /home/dhp
# sudo chown -R dhp:dhp /home/dhp/.ssh
# sudo bash -c 'echo " dhp ALL=(ALL:ALL) NOPASSWORD: ALL"' >> /etc/sudoers 
