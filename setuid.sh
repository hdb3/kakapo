#!/bin/bash -ex
SCRIPTDIR=$(realpath $(dirname "$0"))
pushd $SCRIPTDIR
for f in core/kakapo relay/relay2 `which ip` `find testing/bin -type f -executable` ; do  sudo chown root:root $f ; sudo chmod u+s $f ; done
popd

