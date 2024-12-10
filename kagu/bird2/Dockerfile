FROM ubuntu:24.04
# supply minimal utilities needed for agnostic installation script
# copied from/see -  frr/Dockerfile - for more notes.
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl ca-certificates build-essential flex bison libncurses-dev libreadline-dev
WORKDIR /build
COPY build.sh .
ENV BIRD2VER="2.15.1"
RUN ./build.sh "/usr/bin" "$BIRD2VER"
ENV BGPD="/usr/bin/bird"
# RUN mkdir /config

SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD -d -c /config/bgpd.conf
