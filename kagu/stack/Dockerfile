# this image is plausibly a good intermediate to build cache stack buildsFROM ghcup
# But, only if the stack snapshots are aligned.
# Possibly, ghcup could do some of this directly
WORKDIR /build
COPY Setup.hs simple.cabal Simple.hs stack.yaml /build/
RUN stack update
RUN stack build
ENTRYPOINT ["/bin/bash", "-il"]
