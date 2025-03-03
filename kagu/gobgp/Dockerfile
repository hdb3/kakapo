FROM ubuntu:24.04
# RUN DEBIAN_FRONTEND=noninteractive apt-get update
# RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install wget
# RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl ca-certificates build-essential flex bison libncurses-dev libreadline-dev
WORKDIR /build

# Original version of gobgp was 2.15.0, but under Docker this version segfaults, hence upgraded
# ENV GOVER=2.15.0
# ENV GOVER=3.32.0
ENV GOVER=3.35.0

ADD https://github.com/osrg/gobgp/releases/download/v${GOVER}/gobgp_${GOVER}_linux_amd64.tar.gz .
RUN  tar xf gobgp_${GOVER}_linux_amd64.tar.gz
RUN cp gobgpd /usr/bin
RUN rm -rf ./*
ENV BGPD="/usr/bin/gobgpd"
SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD --log-plain --config-file=/config/bgpd.conf
