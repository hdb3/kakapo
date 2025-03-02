FROM ubuntu:24.04
WORKDIR /build
RUN apt update
RUN DEBIAN_FRONTEND=noninteractive apt -y -y --no-install-recommends install git pkg-config curl ca-certificates build-essential libgmp-dev libffi-dev libncurses-dev zlib1g-dev
RUN DEBIAN_FRONTEND=noninteractive apt -y -y --no-install-recommends install libffi8 libtinfo6

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=y sh
ENV GHC_VER=8.10.7
RUN . ~/.ghcup/env && ghcup install ghc $GHC_VER && ghcup set ghc $GHC_VER
# offer option of latest/later
ENV PATH="$PATH:/root/.ghcup/bin"
ENTRYPOINT ["/bin/bash", "-il"]
