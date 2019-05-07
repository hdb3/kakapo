
// util.c

#include "util.h"

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

// Parsing address parameters
// The subject can be either 192.168.1.2,8.8.8.8 or 192.168.1.2.  If it is the shorter form then the second value is read as if 0.0.0.0.

int parseAddress(char *ss, struct in_addr *dst, struct in_addr *src) {
  // make a local copy becuase we modify it....
  char *s = strdupa(ss);
  char *commaloc = strchr(s, ',');
  if (NULL == commaloc) {
    // default second value to zero / 0.0.0.0
    *src = (struct in_addr){0};
    return inet_aton(s, dst);
  } else {
    *commaloc = 0;
    return (inet_aton(s, dst) | inet_aton(commaloc + 1, src));
  }
}

int die(char *mess) {
  if (0 != errno) {
    fprintf(stderr, "(%d) ", errno);
    perror(mess);
  } else
    fprintf(stderr, "%s\n", mess);
  exit(1);
}

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
