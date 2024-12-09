#!/bin/bash -e
SCRIPTDIR=`realpath $(dirname "$0")`
ln -fs $SCRIPTDIR/nets $HOME/.local/bin
ln -fs $SCRIPTDIR/vm $HOME/.local/bin
echo "installed nets and vm helpers in  $HOME/.local/bin"
