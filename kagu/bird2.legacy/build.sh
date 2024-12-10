#!/bin/bash -xe
TARGET_DIR=${1:?"please give a target directory for build output"}
TARGET_DIR=$(realpath $TARGET_DIR)
mkdir -p ${TARGET_DIR}
BUILD_DIR=$(mktemp -d)
pushd $BUILD_DIR

VERSION=2.15.1
curl https://bird.network.cz/download/bird-${VERSION}.tar.gz | tar xz
cd bird-${VERSION}
./configure --prefix=/
make
cp birdc bird ${TARGET_DIR}
popd
rm -rf ${BUILD_DIR}

