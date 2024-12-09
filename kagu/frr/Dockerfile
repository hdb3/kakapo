FROM fedora:30
# fedora 30 provides FRR 7.3, it is the earliest Fedora which has a package for frr
# FROM fedora:35
# # fedora 35 provides FRR 8.3
RUN dnf -y update && \
    dnf -y install frr && \
    dnf clean all
COPY bgpd.conf /etc/frr/
ENTRYPOINT ["/usr/lib/frr/bgpd", "-S","-Z", "-n", "--log", "stdout", "-f", "/etc/frr/bgpd.conf"]
