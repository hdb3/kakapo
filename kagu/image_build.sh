#!/bin/bash -e

scriptd=$(realpath $(dirname "$0"))

images="ghcup hbgp gobgp bgpd bird1 bird2 bird3 frr"
tagged_build(){
  MD5SUM=$(md5sum $scriptd/$1/Dockerfile | cut -d " " -f1)
  docker build --label dockerfile_md5sum=$MD5SUM -t $1 $scriptd/$1
}

for image in $images
  do tagged_build $image
done
