#!/bin/bash -e
sudo setcap cap_net_bind_service=+ep kakapo
mkdir -p $HOME/.local/bin
cp -vf kakapo $HOME/.local/bin
