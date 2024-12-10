#!/bin/bash -xe
TARGET_DIR=${1:?"please give a target directory for build output"}
TARGET_DIR=$(realpath $TARGET_DIR)
mkdir -p ${TARGET_DIR}
BUILD_DIR=$(mktemp -d)
pushd $BUILD_DIR
VERSION=1.6.8

# NOTE pre 2.0 releases now have specific url with major version in path, e.g. https://bird.network.cz/download/1.6/.....
#      For 2.0 releases the url is https://bird.network.cz/download/bird-${VERSION}.tar.gz

curl https://bird.network.cz/download/1.6/bird-${VERSION}.tar.gz | tar xz
cd bird-${VERSION}

# NOTE More recent versions of gcc require the flag '-fcommon' to compile pre 2.0 releases of bird,
#      in order to prevent duplicate symbol errors at link stage.

./configure --prefix=/ CFLAGS='-fcommon'
make
cp birdc bird ${TARGET_DIR}
popd
rm -rf ${BUILD_DIR}
