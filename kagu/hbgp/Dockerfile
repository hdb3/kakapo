FROM ghcup.ubuntu
WORKDIR /build
# Note - unrecorded motivation for switching between murmur-hash and farmhash.
# It is believed that there is no functional distinction, just package compatibility driven.
# ARG BRANCH=murmur-hash
ARG BRANCH=master
ADD https://api.github.com/repos/hdb3/hBGP/git/refs/heads/${BRANCH} hbgp.json
RUN apt -y -y --no-install-recommends install git pkg-config
RUN \
    git clone https://github.com/hdb3/hBGP.git -b ${BRANCH} && \
    cd hBGP && \
    cabal build && \
    cabal install exe:hbgp exe:hbgp.event --installdir=/build/bin --install-method=copy --overwrite-policy=always

# # FROM fedora:41
# FROM ubuntu:24.04
COPY --from=0 /build/bin/hbgp /usr/bin
COPY --from=0 /build/bin/hbgp.event /usr/bin
COPY --from=0 /build/hbgp.json /root
COPY bgp.conf /root
ENTRYPOINT ["/bin/bash", "-il"]
