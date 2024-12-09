FROM stack
WORKDIR /build
ARG BRANCH=IntPrefix
#ARG BRANCH=DEBUG
#ARG BRANCH=master
#ARG RESOLVER=lts-11.14
#ARG RESOLVER=lts-12.26
ARG RESOLVER=lts-13.25
ADD https://api.github.com/repos/hdb3/hBGP/git/refs/heads/${BRANCH} hbgp.json
RUN git clone https://github.com/hdb3/hBGP.git -b ${BRANCH} && cd hBGP && stack --resolver $RESOLVER build && stack --resolver $RESOLVER install
FROM base
COPY --from=0 /root/.local/bin/hbgp /usr/bin/
COPY --from=0 /build/hbgp.json /root
COPY run.sh /root
COPY bgp.conf /root
ENTRYPOINT ["/bin/bash", "-e", "/root/run.sh"]
