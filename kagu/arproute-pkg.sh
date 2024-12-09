#!/bin/bash -ex
echo "building the arproute install packagearproute.tgz"
docker build -t arprouter arprouter
docker run --rm -i --entrypoint /bin/bash arprouter -c "cat /root/arproute.tgz" > arproute.tgz
ls -l arproute.tgz
tar tzf arproute.tgz
echo "done"
