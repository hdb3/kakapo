# base
# standard development tools
# use as base for e.g. a ghc/ghcup/stack/cabal environment
# if no build required, just use fedora:41
FROM fedora:41
WORKDIR /build
RUN dnf -y install deltarpm 
RUN dnf -y update 
RUN dnf -y install @development-tools
RUN dnf -y install ncurses-libs ncurses-devel gmp-devel wget which zlib-devel readline-devel dmidecode iproute iputils fping openssh-clients rsync
# bird build dependencies
RUN dnf -y install flex bison
# bgpd build dependencies
RUN dnf -y install libevent-devel
# explicit ghcup named dependencies
RUN dnf -y install gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl pkg-config
RUN dnf clean all
ENTRYPOINT ["/bin/bash", "-il"]
