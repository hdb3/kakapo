#!/bin/bash -xe
# Build docler application instances and install in local registry for rapid access
shopt -s expand_aliases
alias docker='DOCKER_BUILDKIT=1 DOCKER_HOST=127.0.0.1 docker'
REPO="localrepo:5000"
baseimages="base ghcup stack"
exportimages="kakapo relay hbgp bird2 bird frr bgpd"

# TODO - why is gobgp missing?

# TODO - check for image existence and don't rebuild,
# only rebuild  based on override parameter on command line

## for i in $baseimages $exportimages ; do docker build -t $i $i ; done
## for ei in $exportimages ; do docker tag ${ei} ${REPO}/${ei}; docker push ${REPO}/${ei} ; done

for bi in $baseimages ; do docker build -t $bi $bi ; done
for ei in $exportimages ; do docker build -t $ei $ei ; docker tag ${ei} ${REPO}/${ei}; docker push ${REPO}/${ei} ; done
