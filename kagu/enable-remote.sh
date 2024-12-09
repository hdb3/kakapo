sudo sed -i -e '/^ExecStart/ s/$/ -H tcp:\/\/0.0.0.0:2375/' /usr/lib/systemd/system/docker.service
sudo systemctl daemon-reload
sudo systemctl restart docker
echo "alias docker='DOCKER_BUILDKIT=1 DOCKER_HOST=127.0.0.1 docker'" >> ~/.bash_aliases
