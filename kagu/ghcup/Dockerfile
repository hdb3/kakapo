FROM ubuntu:24.04
# Note the hbgp stack reseolver version dictates the required ghc version
# lts-23.11 matches ghc 9.8.4
ARG GHC_VER=9.8.4
WORKDIR /build
RUN apt update
RUN DEBIAN_FRONTEND=noninteractive apt -y -y --no-install-recommends install git pkg-config curl ca-certificates build-essential libgmp-dev libffi-dev libncurses-dev zlib1g-dev libffi8 libtinfo6
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=y BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VER BOOTSTRAP_HASKELL_ADJUST_BASHRC=y sh
ENV PATH="$PATH:/root/.ghcup/bin"
ENTRYPOINT ["/bin/bash", "-il"]
