FROM base
WORKDIR /build
# RUN rpm --import /etc/pki/rpm-gpg/*
# RUN dnf -y install deltarpm 
# RUN dnf -y update 
# RUN dnf -y install @development-tools
# RUN dnf -y install ncurses-libs ncurses-devel gmp-devel wget which zlib-devel 

# # ghcup named dependencies
# RUN dnf -y install gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl pkg-config
# # RUN rpm --import /etc/pki/rpm-gpg/*
# RUN dnf clean all
# install GHC build tools
# sudo apt -y -y --no-install-recommends install build-essential curl libffi-dev libgmp-dev libgmp10 libncurses-dev pkg-config
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=y sh
# RUN source ~/.ghcup/env

RUN source ~/.ghcup/env && \
    ghcup  install 8.8.3 && \
    ghcup  install 8.10.1 && \
    ghcup set 8.10.1 && \
    ghcup install-cabal 3.2.0.0
RUN /root/.ghcup/bin/cabal update
ENV PATH="$PATH:/root/.ghcup/bin"
ENTRYPOINT ["/bin/bash", "-il"]
