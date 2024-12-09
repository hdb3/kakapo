#!/bin/bash -ex

get_pkg() {
  mkdir -p deb
  mkdir -p files
  pushd deb
  apt download $1
  popd
  dpkg -X deb/$1* files
}
  

get_pkgs() {
  for pkg in frr gobgpd
  do
    get_pkg $pkg
  done
}
  

main() {
  get_pkgs
  pushd files
  tar cfvP ../pkgs.tar *
  popd
  mv pkgs.tar $1
}


CALLER=$PWD
WORKDIR=`mktemp -d`
pushd $WORKDIR
main $CALLER
popd
rm -rf $WORKDIR

mkdir -p bin/frr bin/gobgp
tar xfv pkgs.tar --strip-components=3 --one-top-level=bin/frr usr/lib/frr/bgpd
tar xfv pkgs.tar --strip-components=2 --one-top-level=bin/gobgp usr/bin/gobgpd

echo "now:

sudo tar  xvf ../kagu/pkgs.tar -C/ usr/lib/x86_64-linux-gnu/frr/

"
