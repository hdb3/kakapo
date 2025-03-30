FROM ubuntu:24.04
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl ca-certificates build-essential flex bison libncurses-dev libreadline-dev
WORKDIR /build
COPY build.sh .
ENV BIRD2VER="3.0.1"
RUN ./build.sh "/usr/bin" "$BIRD2VER"
ENV BGPD="/usr/bin/bird"
# RUN mkdir /config

SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD -d -c /config/bgpd.conf
