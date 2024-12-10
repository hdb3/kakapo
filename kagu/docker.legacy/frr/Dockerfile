FROM ubuntu:24.04
# supply minimal utilities needed for agnostic installation script
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl lsb-release
WORKDIR /build
COPY build.sh .
ENV FRRVER="frr-8"
RUN ./build.sh "$FRRVER"
ENV BGPD="/usr/lib/frr/bgpd"
ENV BGPLISTENADDR="intentionally_invalid_string.0.0.0.0.0"
RUN mkdir /config

# ENTRYPOINT ["$BGPD", "-S","-Z", "-n", "--log", "stdout", "--listenon","$BGPLISTENADDR" "-f", "/config/bgpd.conf"]

# above 'exec' form is broken due to not substituting the environment variables
# this is a 'cheat', needs the SHELL term to avoid scary warnings from docker build.
# Ideally, some other solution which keeps signals should be found, see
# https://docs.docker.com/reference/build-checks/json-args-recommended/
SHELL ["/bin/bash", "-c"]
ENTRYPOINT $BGPD -S -Z -n --log stdout --listenon $BGPLISTENADDR -f /config/bgpd.conf
