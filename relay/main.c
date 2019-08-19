
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
#define FLAGS(a, b, c)
//#define FLAGS( a , b , c) flags( a , b, c)

#define SOCKADDRSZ (sizeof(struct sockaddr_in))
#define BUFSIZE (1024 * 1024 * 64)
#define MINREAD 4096
#define MAXPENDING 2 // Max connection requests

char VERSION[] = "1.2.0";

struct peer {
  int sock;
  uint64_t nread, nwrite, nprocessed;
  int msg_counts[5];
  struct sockaddr_in remote, local;
  void *buf;
};

void zeropeer(struct peer *p) {
  int i;
  p->nread = 0;
  p->nwrite = 0;
  p->nprocessed = 0;
  for (i = 0; i < 5; p->msg_counts[i++] = 0)
    ;
};

char *showpeer(char *pn, struct peer *p) {
  printf("peer: %s local: %s ", pn, inet_ntoa(p->local.sin_addr));
  printf("remote: %s\n", inet_ntoa(p->remote.sin_addr));
};

void peer_report(struct peer *p) {
  printf("\npeer report\n");
  showpeer("", p);
  printf("%ld/%ld/%ld bytes read/written/processed\n", p->nread, p->nwrite, p->nprocessed);
  printf("%d messages\n", *(p->msg_counts));
  printf("%d Opens\n", (p->msg_counts)[1]);
  printf("%d Updates\n", (p->msg_counts)[2]);
  printf("%d Notifications\n", (p->msg_counts)[3]);
  printf("%d Keepalives\n", (p->msg_counts)[4]);
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

  0 == bind(p->sock, (struct sockaddr *)&(p->local), SOCKADDRSZ) ||
      die("Failed to bind local address");
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
    FLAGS(p->sock, __FILE__, __LINE__);
    res = readv(p->sock, iovecs, niovec);
    FLAGS(p->sock, __FILE__, __LINE__);

    if (res > 0) {
      p->nread += res;
      return 0;
    };

    if (errno == EAGAIN) { // nothing available but not an error
                           // should not happen as we only read if the select showed read will succceed
                           // **** HOWEVER *******
                           // experience shows that this does occur at the end of sessions
                           // _repeatedly_
                           // so, until resolved, it is best not to log it
      // printf("unproductive read on stream on fd %d\n", p->sock);
      return 0;
    };

    // all other outcomes are terminal
    // - and the non-zero return causes the session to exit

    if (res == 0) // normal end-of-stream
      printf("end of stream on fd %d\n", p->sock);
    else
      printf("end of stream on fd %d, errno: %d\n", p->sock, errno);

    return 1;

  } else // this is the case where we cannot read because the buffer is full
    return 0;
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
  (*(p->msg_counts))++;
  (*(p->msg_counts + msg_type))++;
  // notice: if counts are zeroed out at connection time then the Open and Notification counts can be used as flags for state changes
  // an EndOfRIB detector would be simple too, by checking for specific msg_length and msg_type
};

void processaction(struct peer *p) {
  // need at least 19 bytes to have a valid length field and a message type in a message....
  uint64_t available;
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

// probapaly TODO remove the flags entirely from the actions
int writeaction(struct peer *p, int sock2, fd_set *set) {
  int res;
  int niovec;
  struct iovec iovecs[2];
  if (0 < p->nprocessed - p->nwrite) {
    // try write in case we just got read afeteer the last select()
    // if ( FD_ISSET ( sock2 , set) && (0 < p->nread - p->nwrite) ) {
    niovec = setupIOVECs(iovecs, p->buf, p->nwrite, p->nprocessed);
    FLAGS(sock2, __FILE__, __LINE__);
    res = writev(sock2, iovecs, niovec);
    FLAGS(sock2, __FILE__, __LINE__);
    if (res > 0) {
      p->nwrite += res;
    } else if (res == 0) // probably an error!!!!
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
    FD_SET(p->sock, rset);
  else
    FD_CLR(p->sock, rset);
};

void run(struct peer *peer1, struct peer *peer2) {
  printf("run\n");
  showpeer("peer1", peer1);
  showpeer("peer2", peer2);
  zeropeer(peer1);
  zeropeer(peer2);
  fd_set rset, wset;
  FD_ZERO(&rset);
  FD_SET(peer1->sock, &rset);
  FD_SET(peer2->sock, &rset);
  FD_ZERO(&wset);
  FD_SET(peer1->sock, &wset);
  FD_SET(peer2->sock, &wset);
  while (1) {
    int res = select(FD_SETSIZE, &rset, &wset, NULL, NULL);
    if (readaction(peer1, &rset))
      break;
    else
      processaction(peer1);
    if (readaction(peer2, &rset))
      break;
    else
      processaction(peer2);
    if (writeaction(peer1, peer2->sock, &wset))
      break;
    if (writeaction(peer2, peer1->sock, &wset))
      break;
    setflags(peer1, peer2->sock, &rset, &wset);
    setflags(peer2, peer1->sock, &rset, &wset);
  };
  peer_report(peer1);
  peer_report(peer2);
};

void setsocketnonblock(int sock) {
  fcntl(sock, F_SETFL, O_NONBLOCK);
};

void setsocketnodelay(int sock) {
  int i = 1;
  setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
};

void serveraccept(int serversock, struct peer *p) {
  struct sockaddr_in acceptaddr;
  int peersock;
  socklen_t socklen;
  memset(&acceptaddr, 0, SOCKADDRSZ);
  socklen = SOCKADDRSZ;
  0 < (peersock = accept(serversock, (struct sockaddr *)&acceptaddr, &socklen)) || die("Failed to accept peer connection");
  (SOCKADDRSZ == socklen && AF_INET == acceptaddr.sin_family) || die("bad sockaddr");
  socklen = SOCKADDRSZ;
  0 == (getpeername(peersock, (struct sockaddr *)&p->remote, &socklen)) || die("Failed to get peer address");
  socklen = SOCKADDRSZ;
  0 == (getsockname(peersock, (struct sockaddr *)&p->local, &socklen)) || die("Failed to get local address");
  p->buf = malloc(BUFSIZE);
  p->sock = peersock;
};

void prepsocket(int sock) {
  fcntl(sock, F_SETFL, O_NONBLOCK);
  setsocketnonblock(sock);
  setsocketnodelay(sock);
};

void serverstart(int serversock, struct peer *peer1, struct peer *peer2) {

  struct in_addr listener_address = peer1->remote.sin_addr;

  printf("server start\n");
  serveraccept(serversock, peer1);
  serveraccept(serversock, peer2);
  prepsocket(peer1->sock);
  prepsocket(peer2->sock);
  if (0 == listener_address.s_addr || peer1->remote.sin_addr.s_addr == listener_address.s_addr)
    // either no explicit listener was given, or it was peer1
    run(peer1, peer2);
  else if (peer2->remote.sin_addr.s_addr == listener_address.s_addr)
    // an explicit listener was given, and it was peer2
    run(peer2, peer1);
  else
    printf("error: the defined listener did not connect\n");
  close(peer1->sock);
  close(peer2->sock);
};

int start(struct peer *peer1, struct peer *peer2) {
  int i;

  printf("client start\n");
  connectPeer(peer1);
  connectPeer(peer2);
  prepsocket(peer1->sock);
  prepsocket(peer2->sock);
  EINPROGRESS != (connect(peer1->sock, (struct sockaddr *)&peer1->remote, SOCKADDRSZ)) || die("Failed to start connect with peer1");
  EINPROGRESS != (connect(peer2->sock, (struct sockaddr *)&peer2->remote, SOCKADDRSZ)) || die("Failed to start connect with peer2");
  FLAGS(peer1->sock, __FILE__, __LINE__);
  FLAGS(peer2->sock, __FILE__, __LINE__);
  int res = waitonconnect(peer1->sock, peer2->sock);
  FLAGS(peer1->sock, __FILE__, __LINE__);
  FLAGS(peer2->sock, __FILE__, __LINE__);
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

void client(char *s1, char *s2) {
  struct peer peer1, peer2;
  initPeer(s1, &peer1);
  initPeer(s2, &peer2);
  while (1) {
    while (0 != start(&peer1, &peer2)) {
      printf("retrying\n");
      sleep(3);
    };
  };
};

void server(char *s) {
  int serversock;
  //struct sockaddr_in serversocketaddress;
  struct sockaddr_in hostaddr = {AF_INET, htons(179), (struct in_addr){0}};
  struct peer peer1, peer2;
  int reuse;

  // 0 < (inet_aton(s, &hostaddr.sin_addr)) || die("failed parsing server listen address");

  // allow the server mode to spevify which peer is intended as the target for Update readvertisments
  // by implication the others are 'receive only', whilst this one 'send only'

  0 != parseAddress(s, &(hostaddr.sin_addr), &(peer1.remote.sin_addr)) ||
      die("Failed to parse addresses");

  0 < (serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) || die("Failed to create socket");

  reuse = 1;
  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEADDR");

  reuse = 1;
  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEPORT");

  0 == (bind(serversock, (struct sockaddr *)&hostaddr, SOCKADDRSZ)) || die("Failed to bind the server socket");

  0 == (listen(serversock, MAXPENDING)) || die("Failed to listen on server socket");

  while (1) {
    serverstart(serversock, &peer1, &peer2);
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
  if (argc == 2) {
    version(argv[1]);
    server(argv[1]);
  } else if (argc == 3)
    client(argv[1], argv[2]);
  else
    die("expecting just one or two arguments");
};
