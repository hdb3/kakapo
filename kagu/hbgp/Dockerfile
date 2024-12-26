FROM ghcup
WORKDIR /build

# Note - the motivation for switching between murmur-hash and farmhash is unrecorded,
# though it is believed that there is no functional distinction, just package version compatibility driven.
# the branch 'master' uses 'farmhash' in place of 'murmur-hash'
# ARG BRANCH=murmur-hash
ARG BRANCH=yaml_config
RUN apt -y -y --no-install-recommends install git pkg-config
ADD https://github.com/hdb3/hBGP.git#${BRANCH} .
RUN ghcup set ghc 9.10.1 && ghcup set cabal 3.12.1.0
RUN cabal build

#  NOTE - the following installs _all_ the versions of
# hbgp - i.e. hbgp.info, hbgp.warn, hbgp.debug, and hbgp.event
# I don't understand why, but as it works i laeve it as it is.

RUN cabal install exe:hbgp.event --installdir=/build/bin --install-method=copy --overwrite-policy=always

RUN cp /build/bin/hbgp.*  /usr/bin
ENV BGPD=/usr/bin/hbgp.info
SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD /config/bgpd.conf
