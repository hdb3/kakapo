FROM base
RUN export GOVER=2.15.0 ; \
    yum -y install wget && \
    wget https://github.com/osrg/gobgp/releases/download/v${GOVER}/gobgp_${GOVER}_linux_amd64.tar.gz && \
    tar xf gobgp_${GOVER}_linux_amd64.tar.gz && \
    cp gobgpd /sbin
COPY gobgpd.conf /etc/
ENTRYPOINT ["/sbin/gobgpd","-p","-f","/etc/gobgpd.conf"]
