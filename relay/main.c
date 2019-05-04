
/* kakapo-relay - a BGP traffic relay */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>

#include <stdint.h>

#include "util.h"


#define SOCKADDRSZ (sizeof(struct sockaddr_in))

struct peer {
  int sock;
  struct sockaddr_in remote, local;
};

int getPeer(char *s, struct peer *p) {

  p->remote = (struct sockaddr_in) {AF_INET, htons(179), (struct in_addr){0}};
  p->local = (struct sockaddr_in) {AF_INET, htons(0), (struct in_addr){0}};
  0 != parseAddress(s, &(p->remote.sin_addr) , &(p->local.sin_addr) ) ||
      die("Failed to parse addresses");

  0 < (p->sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) ||
      die("Failed to create socket");

  0 == bind(p->sock, &(p->local), SOCKADDRSZ) ||
      die("Failed to bind local address");

};

void waitonready (int fd1, int fd2 ) {
  fd_set set, tset;
  struct timeval timeout = {1,0};
  struct timeval notime = {0,0};

  FD_ZERO (&set);
  FD_SET (fd1, &set);
  FD_SET (fd2, &set);

  tset = set;
  while ( 2 > select (FD_SETSIZE, &tset, NULL, NULL, &notime)) {
      if ( FD_ISSET ( fd1 , &tset)) {
          tset = set;
          FD_CLR ( fd1 , &tset);
      } else if ( FD_ISSET ( fd2 , &tset)) {
          tset = set;
          FD_CLR ( fd2 , &set);
      }
      select (FD_SETSIZE, &tset, NULL, NULL, &timeout);
      tset = set;
  };
};

void run(struct peer* peer1, struct peer* peer2) {
    printf("run\n");
    fcntl (peer1->sock, F_SETFL, O_NONBLOCK);
    fcntl (peer2->sock, F_SETFL, O_NONBLOCK);
    EINPROGRESS != (connect(peer1->sock, &peer1->remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer1");
    EINPROGRESS != (connect(peer2->sock, &peer2->remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer2");
    waitonready(peer1->sock,peer2->sock);
    printf("connected\n");
    //while (1)
      //sleep(100);
};

int main(int argc, char *argv[]) {

  struct peer peer1, peer2;

  if ( argc != 3 ) die("expecting just two arguments");

  if ( 0 != getPeer(argv[1],&peer1)) die("could not intialise peer for arg 1");
  if ( 0 != getPeer(argv[2],&peer2)) die("could not intialise peer for arg 2");
  run(&peer1, &peer2);
}
