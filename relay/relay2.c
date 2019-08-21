
/* kakapo-relay2 - a BGP traffic relay */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>

#include "util.h"
#define FLAGS(a, b, c)

#define SOCKADDRSZ (sizeof(struct sockaddr_in))
#define BUFSIZE (1024 * 1024 * 64)
#define MINREAD 4096
#define MAXPEERS 100
#define MAXPENDING 2 // Max connection requests

char VERSION[] = "2.0.0";

struct peer {
  int peer_index, sock;
  uint64_t nread, nwrite, nprocessed;
  int msg_counts[5];
  struct sockaddr_in remote, local;
  void *buf;
} peer_table[MAXPEERS];
int peer_count = 0;
int nfds = 0;
int running = 1;
int listen_sock = -1;

void showpeer(struct peer *p) {
  printf("peer %2d: local: %s ", p->peer_index, inet_ntoa(p->local.sin_addr));
  printf("         remote: %s\n", inet_ntoa(p->remote.sin_addr));
};

void peer_report(struct peer *p) {
  printf("\npeer report\n");
  showpeer(p);
  printf("%ld/%ld/%ld bytes read/written/processed\n", p->nread, p->nwrite, p->nprocessed);
  printf("%d messages\n", *(p->msg_counts));
  printf("%d Opens\n", (p->msg_counts)[1]);
  printf("%d Updates\n", (p->msg_counts)[2]);
  printf("%d Notifications\n", (p->msg_counts)[3]);
  printf("%d Keepalives\n", (p->msg_counts)[4]);
};

int setupIOVECs(struct iovec *vec, void *base, uint64_t start, uint64_t end) {

  if ((end / BUFSIZE) > (start / BUFSIZE) && (end % BUFSIZE) != 0) {
    vec[0].iov_base = base + (start % BUFSIZE);
    vec[0].iov_len = BUFSIZE - (start % BUFSIZE);
    vec[1].iov_base = base;
    vec[1].iov_len = end % BUFSIZE;
    return 2;
  } else {
    vec[0].iov_base = base + (start % BUFSIZE);
    vec[0].iov_len = end - start;
    vec[1].iov_base = NULL;
    vec[1].iov_len = 0;
    return 1;
  };
};

int write_from(struct peer *p, int sock, int length) {
  int res;
  int niovec;
  struct iovec iovecs[2];
  niovec = setupIOVECs(iovecs, p->buf, p->nwrite, p->nwrite + length);
  res = writev(sock, iovecs, niovec);
  if (res == length) { // happy path
    p->nwrite += res;
    return 1;
  } else {
    printf("write error, errno: %d (%d)\n", errno, res);
    return 0;
  };
};

int forward(struct peer *p, int length) {
  write_from(p, listen_sock, length);
};

int echo(struct peer *p, int length) {
  write_from(p, p->sock, length);
};

int stop(struct peer *p, int length) {
  p->nwrite += length;
  running = 0;
  printf("stopping\n");
};

int canRead(int fd, uint64_t nread, uint64_t nwrite) {
  int p = (MINREAD < BUFSIZE + nwrite - nread) ? 1 : 0;
  return p;
  if (p) {
    printf("fd%d - CAN READ %ld (%ld %ld)\n", fd, BUFSIZE + nwrite - nread, nread, nwrite);
  } else {
    printf("fd%d - CANNOT READ %ld (%ld %ld)\n", fd, BUFSIZE + nwrite - nread, nread, nwrite);
  };
  return p;
};

void processaction(struct peer *p);
int readaction(struct peer *p, int read_flag) {
  int res;
  int niovec;
  struct iovec iovecs[2];

  if (read_flag && canRead(p->sock, p->nread, p->nwrite)) {
    niovec = setupIOVECs(iovecs, p->buf, p->nread, p->nwrite + BUFSIZE);
    res = readv(p->sock, iovecs, niovec);

    if (res == -1) { // some kind of error
      if (errno != EAGAIN) {
        printf("peer %d: end of stream on fd %d, errno: %d\n", p->peer_index, p->sock, errno);
        running = 0;
      } else
        ;                  // (errno == EAGAIN)
                           // nothing available but not an error
                           // should not happen as we only read if the select showed read will succceed
                           // **** HOWEVER *******
                           // experience shows that this does occur at the end of sessions
                           // _repeatedly_
                           // so, until resolved, it is best not to log it
    } else if (res == 0) { // normal end-of-stream
      printf("peer %d: end of stream on fd %d\n", p->peer_index, p->sock);
      running = 0;
    } else if (res > 0) {
      p->nread += res;
      processaction(p);
    } else { // impossible to have -ve != -1
      printf("peer %d: impossible end of stream(%d) on fd %d\n", p->peer_index, res, p->sock);
      running = 0;
    };
  } else // this is either the (unexpected) case where we cannot read because the buffer is full
         // or more likely simply that read flag is unset and this is our chance to set it again
    ;    // printf("peer %d: impossible invocation on fd %d\n", p->peer_index, p->sock);
  return canRead(p->sock, p->nread, p->nwrite);
};

uint8_t *get_byte(struct peer *p, uint64_t offset) {
  return (p->buf) + ((offset + p->nprocessed) % BUFSIZE);
};

void dpi(struct peer *p, uint8_t msg_type, uint16_t msg_length) {
  // this function is the opportunity to do as much or as little mangling as you might need
  // but it is inline with IO so best not take too long.....
  // for heavy processing it might be better to copy to a contiguous buffer
  // and return immeddiately
  (5 > msg_type) || (1 < msg_type) || die("bad sync");
  (4097 > msg_length) || (19 < msg_length) || die("bad sync2");
  (*(p->msg_counts))++;
  (*(p->msg_counts + msg_type))++;
  // notice: if counts are zeroed out at connection time then the Open and Notification counts can be used as flags for state changes
  // an EndOfRIB detector would be simple too, by checking for specific msg_length and msg_type
  switch (msg_type) {
  case 1:
    printf("peer %d, Open\n", p->peer_index);
    echo(p, msg_length);
    break;
  case 2:
    forward(p, msg_length);
    break;
  case 3:
    printf("peer %d, Notification\n", p->peer_index);
    stop(p, msg_length);
    break;
  case 4:
    printf("peer %d, Keepalive\n", p->peer_index);
    echo(p, msg_length);
    break;
  };
};

void processaction(struct peer *p) {
  // need at least 19 bytes to have a valid length field and a message type in a message....
  uint64_t available;
  // uncomment to remove message delineation
  // p->nprocessed = p->nread;
  available = p->nread - p->nprocessed;
  while (18 < available) {
    uint16_t msg_length = (*(get_byte(p, 16)) << 8) + *get_byte(p, 17);
    uint8_t msg_type = *get_byte(p, 18);
    if (msg_length <= available) {
      dpi(p, msg_type, msg_length);
      p->nprocessed += msg_length;
      available -= msg_length;
    } else
      break;
  };
};

void run() {
  int res, n, flag;
  struct peer *p;
  fd_set read_set;
  fd_set write_set;

  printf("run\n");
  FD_ZERO(&read_set);
  FD_ZERO(&write_set);
  while (running) {
    for (n = 0; n < peer_count; n++) {
      p = peer_table + n;
      flag = FD_ISSET(p->sock, &read_set);
      if (readaction(p, flag))
        FD_SET(p->sock, &read_set);
      else
        FD_CLR(p->sock, &read_set);
    };
    res = select(nfds, &read_set, &write_set, NULL, NULL);
  };
  for (n = 0; n < peer_count; p = &peer_table[n++])
    peer_report(p);
};

void setsocketnonblock(int sock) {
  fcntl(sock, F_SETFL, O_NONBLOCK);
};

void setsocketnodelay(int sock) {
  int i = 1;
  setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
};

void server(char *s1, char *s2) {
  struct sockaddr_in acceptaddr;
  int peersock;
  socklen_t socklen;
  struct peer *p;
  struct sockaddr_in host = {AF_INET, htons(179), (struct in_addr){0}};
  int serversock;
  int reuse;
  struct in_addr listener_addr;
  pthread_t threadid;

  printf("server start\n");

  0 != (inet_aton(s1, &host.sin_addr)) || die("failed parsing server listen address");
  0 != (inet_aton(s2, &listener_addr)) || die("failed parsing listener peer address");

  0 < (serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) || die("Failed to create socket");

  reuse = 1;
  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEADDR");

  reuse = 1;
  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEPORT");

  0 == (bind(serversock, (struct sockaddr *)&host, SOCKADDRSZ)) || die("Failed to bind the server socket");

  0 == (listen(serversock, MAXPENDING)) || die("Failed to listen on server socket");

  while (peer_count < MAXPEERS) {
    p = &peer_table[peer_count];
    memset(p, 0, sizeof(struct peer));
    p->peer_index = peer_count;
    memset(&acceptaddr, 0, SOCKADDRSZ);
    -1 != (peersock = accept(serversock, NULL, NULL)) || die("Failed to accept peer connection");
    socklen = SOCKADDRSZ;
    0 == (getpeername(peersock, (struct sockaddr *)&p->remote, &socklen)) || die("Failed to get peer address");
    socklen = SOCKADDRSZ;
    0 == (getsockname(peersock, (struct sockaddr *)&p->local, &socklen)) || die("Failed to get local address");
    // fcntl(peersock, F_SETFL, O_NONBLOCK);
    // setsocketnonblock(peersock);
    setsocketnodelay(peersock);
    p->buf = malloc(BUFSIZE);
    p->sock = peersock;
    if (p->remote.sin_addr.s_addr == listener_addr.s_addr) {
      printf("listener peer connected\n");
      listen_sock = peersock;
    };
    nfds = peersock + 1 > nfds ? peersock + 1 : nfds;
    showpeer(p);
    peer_count++;
    if (1 == peer_count)
      pthread_create(&threadid, NULL, (void *)run, NULL);
  };
};

void version(char *s) {
  if (s[0] == '-' && ((s[1] == 'v' || s[1] == 'V')) ||
      (s[1] == '-' && (s[2] == 'v' || s[2] == 'V'))) {
    printf("relay version %s\n", VERSION);
    exit(0);
  };
};

int main(int argc, char *argv[]) {
  setlinebuf(stdout);
  prctl(PR_SET_DUMPABLE, 1);
  if (argc == 2) {
    version(argv[1]);
  } else if (argc == 3)
    server(argv[1], argv[2]);
  else
    die("expecting just one or two arguments");
};
