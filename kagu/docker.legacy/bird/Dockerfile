FROM base
WORKDIR /build
ARG VERSION=1.6.8
# NOTE pre 2.0 releases now have specific url with version in path, e.g. https://bird.network.cz/download/1.6/.....
RUN curl https://bird.network.cz/download/1.6/bird-${VERSION}.tar.gz | tar xz && \
    cd bird-${VERSION} && \
    ./configure --prefix=/  CFLAGS='-fcommon' && \
    make && \
    cp birdc bird ..
FROM fedora:41
COPY bird.conf /etc
COPY run.sh /root
COPY --from=0 /build/birdc /build/bird /usr/sbin/
ENTRYPOINT ["/bin/bash", "-e", "/root/run.sh"]
