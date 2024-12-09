#!/bin/bash

# kakapo.sh

# run the kakapo test set

scriptd=$(realpath $(dirname "$0"))

source ${scriptd}/local.sh
source ${scriptd}/common.sh

echo "$(basename \"$0\")"
UPDATES=/home/nic/mrtdata/rrc18.latest-bview.raw
# mrtdump /home/nic/mrtdata/rrc18.latest-bview.gz 10000 > /tmp/updates.10000
# UPDATES=/tmp/updates.10000
KAKAPO="/home/nic/src/kakapo/core/kakapo"
KAKAPOCMD="7.0.0.1,7.0.0.2,65000 7.0.0.1,7.0.0.2,65000"
export SENDFILENAME=${UPDATES} TIMEOUT=1000 MODE=FILE REPEAT=1
# KAKAPOENV="SENDFILENAME=${UPDATES} TIMEOUT=1000 MODE=FILE"
#ENV="SENDFILENAME=/home/nic/src/hBGP.master/MRT/raw/66234.raw TIMEOUT=1000 REPEAT=1 MODE=FILE"
#ENV="SENDFILENAME=updates.raw TIMEOUT=100 REPEAT=3 MODE=FILE"
#ENV="TIMEOUT=60 REPEAT=1 TABLESIZE=160000 GROUPSIZE=5 BLOCKSIZE=1 MODE=SINGLE"
#ENV="BLOCKSIZE=1 MODE=SINGLERATE"

$KAKAPO $KAKAPOCMD
read -p "exit?"
tmux wait -S test-complete
