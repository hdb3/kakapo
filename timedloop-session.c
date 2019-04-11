#include "timedloop.h"
#include "timespec.h"
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>

int mysock, msgcount;

int f(int p) {
  char s[20];
  if (p >= msgcount)
    return 1;
  sprintf(s, "msg %d\n", p);
  send(mysock, s, strlen(s), 0);
  return 0;
};

void session(int sock, int argc, char **argv) {

  mysock = sock;
  int i = 1;
  setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
  int nanosecs;
  if (argc > 0)
    sscanf(argv[0], "%d", &nanosecs);
  else
    nanosecs = 1e9;
  if (argc > 1)
    sscanf(argv[1], "%d", &msgcount);
  else
    msgcount = 10;
  struct timespec duration = {0, nanosecs};
  // struct timespec duration = {1,0};
  timedloop(duration, f);
};
