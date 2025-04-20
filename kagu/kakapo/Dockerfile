FROM ubuntu:24.04
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install build-essential git uuid-dev

WORKDIR /build
RUN --mount=from=kakapo,source=/,target=/kakapo cp -r /kakapo/core /kakapo/relay .
RUN --mount=from=kakapo,source=.git,target=.git cd core && bash -xe build.sh && install kakapo /usr/sbin/

RUN --mount=from=kakapo,source=.git,target=.git cd relay && bash -xe build.sh && install relay2 /usr/sbin/

# ENTRYPOINT ["/bin/bash", "-e", "/root/run.sh"]
ENTRYPOINT ["/bin/bash"]
