#!/bin/bash -e
sudo setcap cap_net_bind_service=+ep relay2
sudo setcap cap_net_bind_service=+ep relay
mkdir -p $HOME/.local/bin
cp -vf relay $HOME/.local/bin
cp -vf relay2 $HOME/.local/bin
