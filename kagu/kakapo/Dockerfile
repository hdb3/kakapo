FROM ubuntu:24.04 AS builder
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install build-essential git uuid-dev

WORKDIR /build
RUN --mount=from=kakapo_src,source=/,target=/kakapo cp -r /kakapo/core /kakapo/relay .
RUN --mount=from=kakapo_src,source=.git,target=.git cd core && bash -xe build.sh
RUN --mount=from=kakapo_src,source=.git,target=.git cd relay && bash -xe build.sh

FROM ubuntu:24.04

# COPY --from=builder /build/core/kakapo /usr/sbin/
# COPY --from=builder /build/relay/relay2 /usr/sbin/

#use 'install' rather than just copy...
RUN --mount=from=builder,source=/build,target=/build install /build/core/kakapo /usr/sbin/
RUN --mount=from=builder,source=/build,target=/build install /build/relay/relay2 /usr/sbin/

ENTRYPOINT ["/usr/sbin/kakapo"]
