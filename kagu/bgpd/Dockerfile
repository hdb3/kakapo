FROM ubuntu:24.04
# supply minimal utilities needed for agnostic installation script
# copied from/see -  frr/Dockerfile - for more notes.
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl ca-certificates build-essential libevent-dev flex bison libncurses-dev libreadline-dev
WORKDIR /build
COPY build.sh .
ENV BGPDVER="8.6"
RUN ./build.sh "/usr/bin" "$BIRD2VER"
# surely there is a simpler way to create a user, the restricted privilege hardly matters in a container
RUN  groupadd _bgpd && \
     useradd -g _bgpd -s /sbin/nologin -d /empty -c 'OpenBGPD daemon' _bgpd && \
     mkdir -p /empty && \
     chown 0 /empty && \
     chgrp 0 /empty && \
     chmod 0755 /empty
ENV BGPD="/usr/bin/bgpd"
SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD -d -f /config/bgpd.conf
