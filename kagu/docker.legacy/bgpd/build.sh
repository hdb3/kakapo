#!/bin/bash -e
TARGET_DIR=${1:?"please give a target directory for build output"}
TARGET_DIR=$(realpath $TARGET_DIR)

sudo apt install -y --no-install-recommends libevent-dev

mkdir -p ${TARGET_DIR}
BUILD_DIR=$(mktemp -d)
pushd $BUILD_DIR

VER=8.6
curl http://cdn.openbsd.org/pub/OpenBSD/OpenBGPD/openbgpd-${VER}.tar.gz | tar xz
cd openbgpd-${VER}
./configure --sysconfdir=/etc CFLAGS='-fcommon'
make 
cp src/bgpd/bgpd src/bgpctl/bgpctl ${TARGET_DIR}
popd
rm -rf ${BUILD_DIR}

echo "needed config:

groupadd _bgpd
useradd -g _bgpd -s /sbin/nologin -d /empty -c 'OpenBGPD daemon' _bgpd
mkdir -p /empty
chown 0 /empty
chgrp 0 /empty
chmod 0755 /empty
"
