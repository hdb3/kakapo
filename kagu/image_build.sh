#!/bin/bash -e

images="base bird2 frr gobgp bgpd"
images="ghcup hbgp gobgp bgpd bird bird2 frr"
tagged_build(){

  MD5SUM=$(md5sum $1/Dockerfile | cut -d " " -f1)
  docker build --label dockerfile_md5sum=$MD5SUM -t $1 $1
}

for image in $images
  do tagged_build $image
done
