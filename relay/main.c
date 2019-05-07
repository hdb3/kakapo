
/* kakapo-relay - a BGP traffic relay */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include <stdint.h>
#include <sys/uio.h>

#include "util.h"

#define SOCKADDRSZ (sizeof(struct sockaddr_in))
//#define BUFSIZE ( 1024 * 1024 )
#define BUFSIZE (1024 * 1024 * 64)
#define MINREAD 4096

struct peer {
  int sock, nread, nwrite;
  struct sockaddr_in remote, local;
  void *buf;
};

void initPeer(char *s, struct peer *p) {

  p->remote = (struct sockaddr_in){AF_INET, htons(179), (struct in_addr){0}};
  p->local = (struct sockaddr_in){AF_INET, htons(0), (struct in_addr){0}};
  0 != parseAddress(s, &(p->remote.sin_addr), &(p->local.sin_addr)) ||
      die("Failed to parse addresses");

  p->buf = malloc(BUFSIZE);
};

void connectPeer(struct peer *p) {

  0 < (p->sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) ||
      die("Failed to create socket");

  0 == bind(p->sock, &(p->local), SOCKADDRSZ) ||
      die("Failed to bind local address");
  p->nread = 0;
  p->nwrite = 0;
};

int waitonconnect(int fd1, int fd2) {
  fd_set set, tset;
  struct timeval timeout = {10, 0};
  struct timeval notime = {0, 0};

  FD_ZERO(&set);
  FD_SET(fd1, &set);
  FD_SET(fd2, &set);

  tset = set;
  while (2 > select(FD_SETSIZE, NULL, &tset, NULL, &notime)) {
    if (FD_ISSET(fd1, &tset)) {
      tset = set;
      FD_CLR(fd1, &tset);
    } else if (FD_ISSET(fd2, &tset)) {
      tset = set;
      FD_CLR(fd2, &set);
    }
    select(FD_SETSIZE, NULL, &tset, NULL, &timeout);
    tset = set;
  };
  int err, szerr;
  szerr = sizeof(err);
  getsockopt(fd1, SOL_SOCKET, SO_ERROR, (void *)&err, &szerr);
  if (szerr != sizeof(err) || err != 0) {
    printf("connect error on fd1 (%d)\n", err);
    return -1;
  };
  szerr = sizeof(err);
  getsockopt(fd2, SOL_SOCKET, SO_ERROR, (void *)&err, &szerr);
  if (szerr != sizeof(err) || err != 0) {
    printf("connect error on fd2 (%d)\n", err);
    return -1;
  };
  return 0;
};

void showselectflags(char *ctxt, fd_set *rset, fd_set *wset, int fd1, int fd2) {
  printf("select flags: %s  peer1: [ ", ctxt);
  if (FD_ISSET(fd1, rset))
    printf("READ ");
  if (FD_ISSET(fd1, wset))
    printf("WRITE ");
  printf(" ]\n");
  printf("select flags: %s  peer2: [ ", ctxt);
  if (FD_ISSET(fd2, rset))
    printf("READ ");
  if (FD_ISSET(fd2, wset))
    printf("WRITE ");
  printf(" ]\n");
};

int setupIOVECs(struct iovec *vec, void *base, int start, int end) {

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

int canRead(int fd, int nread, int nwrite) {
  int p = (MINREAD < BUFSIZE + nwrite - nread) ? 1 : 0;
  return p;
  if (p) {
    printf("fd%d - CAN READ %d (%d %d)\n", fd, BUFSIZE + nwrite - nread, nread, nwrite);
  } else {
    printf("fd%d - CANNOT READ %d (%d %d)\n", fd, BUFSIZE + nwrite - nread, nread, nwrite);
  };
  return p;
};

int readaction(struct peer *p, fd_set *set) {
  int res;
  int niovec;
  struct iovec iovecs[2];
  if (FD_ISSET(p->sock, set) && canRead(p->sock, p->nread, p->nwrite)) {
    //if ( FD_ISSET ( p->sock , set) && (MINREAD < BUFSIZE + p->nwrite - p->nread) ) {
    niovec = setupIOVECs(iovecs, p->buf, p->nread, p->nwrite + BUFSIZE);
    res = readv(p->sock, iovecs, niovec);
    if (res > 0) {
      p->nread += res;
    } else if (res = 0) // normal end-of-stream
      return 1;
    else if (errno == EAGAIN) // nothing available but not an error
      ;                       // printf("got nothing from a read, just saying....\n");
                              // should not happen as we only read if the select showed read will succceed
    else {
      printf("end of stream on fd %d, errno: %d\n", p->sock, errno);
      return 1;
    };
  };
  return 0;
};

// probapaly TODO remove the flags entirely from the actions
int writeaction(struct peer *p, int sock2, fd_set *set) {
  int res;
  int niovec;
  struct iovec iovecs[2];
  if (0 < p->nread - p->nwrite) {
    // try write in case we just got read afeteer the last select()
    // if ( FD_ISSET ( sock2 , set) && (0 < p->nread - p->nwrite) ) {
    niovec = setupIOVECs(iovecs, p->buf, p->nwrite, p->nread);
    res = writev(sock2, iovecs, niovec);
    if (res > 0) {
      p->nwrite += res;
    } else if (res = 0) // probably an error!!!!
      return 1;
    else if (errno == EAGAIN)
      ;
    else {
      printf("write error, errno: %d\n", errno);
      return 1;
    };
  };
  return 0;
};

int setflags(struct peer *p, int sock2, fd_set *rset, fd_set *wset) {
  if (0 < p->nread - p->nwrite)
    FD_SET(sock2, wset);
  else
    FD_CLR(sock2, wset);

  if (canRead(p->sock, p->nread, p->nwrite))
    //if (MINREAD < BUFSIZE + p->nwrite - p->nread)
    FD_SET(p->sock, rset);
  else
    FD_CLR(p->sock, rset);
};

void run(struct peer *peer1, struct peer *peer2) {
  printf("run\n");
  printf("fd1=%d fd2=%d\n", peer1->sock, peer2->sock);
  fd_set rset, wset;
  FD_ZERO(&rset);
  FD_SET(peer1->sock, &rset);
  FD_SET(peer2->sock, &rset);
  FD_ZERO(&wset);
  FD_SET(peer1->sock, &wset);
  FD_SET(peer2->sock, &wset);
  while (1) {
    int res = select(FD_SETSIZE, &rset, &wset, NULL, NULL);
    //showselectflags("after select", &rset, &wset, peer1->sock, peer2->sock);
    //printf("select yielded %d\n", res);
    if (readaction(peer1, &rset))
      break;
    if (readaction(peer2, &rset))
      break;
    if (writeaction(peer1, peer2->sock, &wset))
      break;
    if (writeaction(peer2, peer1->sock, &wset))
      break;
    setflags(peer1, peer2->sock, &rset, &wset);
    setflags(peer2, peer1->sock, &rset, &wset);
    //printf("peer1: %d %d %d\n", peer1->sock, peer1->nread, peer1->nwrite);
    //printf("peer2: %d %d %d\n", peer2->sock, peer2->nread, peer2->nwrite);
    //showselectflags("after action", &rset, &wset, peer1->sock, peer2->sock);
  };
};

int start(struct peer *peer1, struct peer *peer2) {
  int i;

  printf("start\n");
  connectPeer(peer1);
  connectPeer(peer2);
  fcntl(peer1->sock, F_SETFL, O_NONBLOCK);
  fcntl(peer2->sock, F_SETFL, O_NONBLOCK);
  i = 1;
  setsockopt(peer1->sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
  i = 1;
  setsockopt(peer2->sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
  EINPROGRESS != (connect(peer1->sock, &peer1->remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer1");
  EINPROGRESS != (connect(peer2->sock, &peer2->remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer2");
  int res = waitonconnect(peer1->sock, peer2->sock);
  if (0 == res) {
    printf("connected\n");
    run(peer1, peer2);
  } else {
    printf("rejected\n");
  };
  close(peer1->sock);
  close(peer2->sock);
  return res;
};

int main(int argc, char *argv[]) {

  struct peer peer1, peer2;

  if (argc != 3)
    die("expecting just two arguments");
  initPeer(argv[1], &peer1);
  initPeer(argv[2], &peer2);

  // while (1) {
  while (0 != start(&peer1, &peer2)) {
    printf("retrying\n");
    sleep(3);
  };
  // };
  printf("all done\n");
}
