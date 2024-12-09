#!/bin/bash

# exabgp.sh

# run the exabgp SUT configuration

EXASCRIPT=/home/nic/src/exa-simple/relay.conf
EXAENV=/home/nic/src/exa-simple/exabgp.env
EXAHOME=/home/nic/src/exabgp
echo "starting exabgp"
$EXAHOME/sbin/exabgp --env $EXAENV $EXASCRIPT
