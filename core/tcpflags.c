
// tcpflags.c

#include "tcpflags.h"

// #include <arpa/inet.h>
#include <assert.h>
#include <stdio.h>
// #include <stdlib.h>
// #include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>

void flag(int flag, char *flagname, int fd, char *file, int line) {
  int lopt, opt, rval;
  lopt = sizeof(opt);
  rval = getsockopt(fd, IPPROTO_TCP, flag, (void *)&opt, &lopt);
  assert(rval == 0);
  assert(lopt == sizeof(opt));
  if (0 == opt) {
    printf("flag %s found cleared at %s:%d\n", flagname, file, line);
    opt = 1;
    lopt = sizeof(opt);
    rval = setsockopt(fd, IPPROTO_TCP, flag, (void *)&opt, lopt);
    assert(rval == 0);
    rval = getsockopt(fd, IPPROTO_TCP, flag, (void *)&opt, &lopt);
    assert(rval == 0);
    assert(lopt == sizeof(opt));
    assert(opt == 1);
  };
};

void flags(int fd, char *file, int line) {
  flag(TCP_NODELAY, "TCP_NODELAY", fd, file, line);
  flag(TCP_QUICKACK, "TCP_QUICKACK", fd, file, line);
};
