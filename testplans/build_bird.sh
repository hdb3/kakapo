#!/bin/bash -ex
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends build-essential flex bison libreadline-dev
WORKDIR=`mktemp -d`
pushd $WORKDIR
VERSION=2.15.1
curl ftp://bird.network.cz/pub/bird/bird-${VERSION}.tar.gz | tar xz
cd bird-${VERSION}
./configure --prefix=/ && make 
BINDIR="$HOME/.local/bin"
mkdir -p $BINDIR
if cp -nv bird birdc birdcl $BINDIR
then
  echo "bird installed in $BINDIR"
  popd
  rm -rf $WORKDIR
else
  echo "bird not installed"
fi
