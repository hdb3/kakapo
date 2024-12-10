FROM base
WORKDIR /build
RUN export VER=8.6 ; curl http://cdn.openbsd.org/pub/OpenBSD/OpenBGPD/openbgpd-${VER}.tar.gz | tar xz && \
    cd openbgpd-${VER} && \
    ./configure --sysconfdir=/etc CFLAGS='-fcommon' && \
    make && \
    cp src/bgpd/bgpd src/bgpctl/bgpctl ..
FROM fedora:41
COPY bgpd.conf /etc
COPY --from=0 /build/bgpd /build/bgpctl /usr/sbin/
RUN  groupadd _bgpd && \
     useradd -g _bgpd -s /sbin/nologin -d /empty -c 'OpenBGPD daemon' _bgpd && \
     mkdir -p /empty && \
     chown 0 /empty && \
     chgrp 0 /empty && \
     chmod 0755 /empty

ENTRYPOINT ["/usr/sbin/bgpd", "-d"]
