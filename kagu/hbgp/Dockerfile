FROM ghcup
WORKDIR /build

ARG BRANCH=main.rc0
RUN DEBIAN_FRONTEND=noninteractive apt -y -y --no-install-recommends install git pkg-config
ADD https://github.com/hdb3/hBGP.git#${BRANCH} .

#  NOTE - the following installs _all_ the versions of
# hbgp - i.e. hbgp.info, hbgp.warn, hbgp.debug, and hbgp.event
# I don't understand why, but as it works i leave it as it is.

RUN stack build
RUN stack install

ENV BGPD=/root/.local/bin/hbgp.info
SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD +RTS -maxN8 -RTS /config/bgpd.conf
