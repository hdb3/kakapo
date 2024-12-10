#!/bin/bash -e
# standalone build script for installing FRR from https://deb.frrouting.org/frr

# useful but not limited to Dockerisation
# built for ubuntu:24.04, but should be compatible with others
FRRVER=${1:-"frr-8"}

# curl and lsb_release should be in any sane system....
# best have bash available too...!
# DEBIAN_FRONTEND=noninteractive apt-get update
# DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl lsb-release

# simple unkeyed version.....
# echo deb https://deb.frrouting.org/frr $(lsb_release -s -c) $FRRVER > /etc/apt/sources.list.d/frr.list

curl -s https://deb.frrouting.org/frr/keys.gpg | tee /usr/share/keyrings/frrouting.gpg > /dev/null
echo deb '[signed-by=/usr/share/keyrings/frrouting.gpg]' https://deb.frrouting.org/frr $(lsb_release -s -c) $FRRVER | tee -a /etc/apt/sources.list.d/frr.list

DEBIAN_FRONTEND=noninteractive apt-get update
DEBIAN_FRONTEND=noninteractive apt-get -y install frr frr-pythontools

# bgpd executable is at  "/usr/lib/frr/bgpd"
# but, it also requires shared libraries elsewhere
# so, best not copy anything to anywhere
