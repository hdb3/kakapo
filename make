#!/bin/bash -e
pushd core
source build.sh
source install.sh
popd
pushd relay
source build.sh
source install.sh
popd
