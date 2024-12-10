FROM base
WORKDIR /build
ARG VERSION=2.15.1
RUN curl https://bird.network.cz/download/bird-${VERSION}.tar.gz | tar xz && \
    cd bird-${VERSION} && \
    ./configure --prefix=/ && \
    make && \
    cp birdc bird ..
FROM fedora:41
COPY bird.conf /etc
COPY --from=0 /build/birdc /build/bird /usr/sbin/
ENTRYPOINT ["/usr/sbin/bird", "-d"]

