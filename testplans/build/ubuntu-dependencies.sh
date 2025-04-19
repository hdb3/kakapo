#!/bin/bash -ex
APTINSTALL="DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends"
sudo apt update
sudo $APTINSTALL \
  uuid-dev \
  net-tools \
  whois \
  git \
  tmux \
  fping \
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
sudo usermod -a $USER -G libvirt
echo 'uri_default = "qemu:///system"' > /etc/libvirt/libvirt.conf

sudo $APTINSTALL ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}") stable" |
  sudo tee /etc/apt/sources.list.d/docker.list >/dev/null
sudo apt-get update
sudo $APTINSTALL docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

sudo usermod -aG docker $USER
sg docker "docker --version"
sg docker "docker run hello-world"
