FROM ubuntu:24.04
ENV FRRVER="frr-8"
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install curl lsb-release
# RUN echo deb https://deb.frrouting.org/frr $(lsb_release -s -c) $FRRVER > /etc/apt/sources.list.d/frr.list.simple
RUN curl -s https://deb.frrouting.org/frr/keys.gpg | tee /usr/share/keyrings/frrouting.gpg > /dev/null
RUN echo deb '[signed-by=/usr/share/keyrings/frrouting.gpg]' https://deb.frrouting.org/frr $(lsb_release -s -c) $FRRVER | \
 tee -a /etc/apt/sources.list.d/frr.list

RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install frr frr-pythontools
# COPY bgpd.conf /etc/frr/
# ENTRYPOINT ["/usr/lib/frr/bgpd", "-S","-Z", "-n", "--log", "stdout", "-f", "/etc/frr/bgpd.conf"]
