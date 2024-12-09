#!/bin/bash -e
#libvirt-daemon libvirt-daemon-system dnsmasq dnsmasq-base qemu-utils qemu-kvm
sudo apt update
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
  net-tools \
  whois \
  git \
  tmux \
  openssh-server \
  build-essential \
  virt-manager \
  libvirt-clients \
  libffi-dev \
  libncurses-dev \
  curl \
  wget \
  tshark \
  wireshark \
  vim-syntastic \
  tcl-expect \
  expect \
  libffi-dev \
  libncurses6 \
  libtinfo6 \
  libvirt-daemon \
  libvirt-daemon-system \
  dnsmasq \
  dnsmasq-base \
  qemu-utils \
  qemu-kvm
if ! which docker > /dev/null
then
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends docker.io
fi

if ! grep 2375 /usr/lib/systemd/system/docker.service > /dev/null
then
  sudo sed -i -e '/^ExecStart/ s/$/ -H tcp:\/\/127.0.0.1:2375/' /usr/lib/systemd/system/docker.service
  sudo systemctl daemon-reload
  sudo systemctl restart docker
  echo "alias docker='DOCKER_HOST=127.0.0.1 docker'" >> ~/.bash_aliases
  alias docker='DOCKER_HOST=127.0.0.1 docker'
fi
sudo usermod -a $USER -G libvirt
# mkdir -p ~/.local/bin
# pushd nets
# source install.sh
# popd
