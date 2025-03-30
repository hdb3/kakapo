#!/bin/bash -e

# export DOCKER_BUILDKIT=1 DOCKER_HOST=127.0.0.1

images="ghcup hbgp gobgp bgpd bird1 bird2 bird3 frr"
tagged_build(){
  MD5SUM=$(md5sum $1/Dockerfile | cut -d " " -f1)
  docker build --label dockerfile_md5sum=$MD5SUM -t $1 $1
}

for image in $images
  do tagged_build $image
done
