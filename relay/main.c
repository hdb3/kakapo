
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
#define BUFSIZE ( 1024 * 1024 )

struct peer {
  int sock, nread, nwrite;
  struct sockaddr_in remote, local;
  void *buf;
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

/// NOTE this is currently wiat on READ ready, i.e. input is avaliable
//       if the connection event itefl is wanted then it would be better to check for the WRITE condition not the READ condition
//       i.e. set the bits in the second paramter to select rather than the first
void waitonconnect (int fd1, int fd2 ) {
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

int action (struct peer* p, fd_set* rset, fd_set* wset) {
    int res;
    if ( p->nread ==0 ) {
    // we are in read mode
        if ( FD_ISSET ( p->sock , rset)) {
            res = read(p->sock, p->buf, BUFSIZE);
            if ( res > 0 ) {
                p->nread = res;
                p->nwrite = 0;
                FD_SET ( p->sock , wset); // set this flag so that the immediare write can happen
            } else if ( res = 0 )  // normal end-of-stream
                return 1;
            else if ( res = EAGAIN )  // nothing available but not an error
                printf("got nothing from a read, just saying....\n");
            else
                die ("unexpected condition in action/read mode");
        };
    };

    if ( p->nread > 0 ) {
    // we are in write mode
        if ( FD_ISSET ( p->sock , wset)) {
            res = write(p->sock, p->buf+p->nwrite, p->nread - p->nwrite);
            if ( res > 0 )
                p->nwrite += res;
            else
                die ("unexpected condition in action/read mode");
            if ( p->nwrite = p->nread )
                p->nread = 0; // signal change of mode
                p->nwrite = 0;
        };
    };

    if (p->nread) {
    // we are now/still in write mode
        FD_SET ( p->sock , wset );
        FD_CLR ( p->sock , rset );
    } else {
    // we are now in read mode
        FD_SET ( p->sock , rset );
        FD_CLR ( p->sock , wset );
    };
    return 0;
};
 
void run(struct peer* peer1, struct peer* peer2) {
    printf("run\n");
    fd_set rset, wset;
    FD_ZERO (&rset); FD_SET (peer1->sock, &rset); FD_SET (peer2->sock, &rset);
    FD_ZERO (&wset); FD_SET (peer1->sock, &wset); FD_SET (peer2->sock, &wset);
    peer1-> buf = malloc(BUFSIZE); peer1-> nread = 0; peer1->nwrite = 0;
    peer2-> buf = malloc(BUFSIZE); peer2-> nread = 0; peer2->nwrite = 0;
    while ( 1 ) {
        int res = select (FD_SETSIZE, &rset, &wset, NULL, NULL);
        printf("select yielded %d\n", res);
        if ( action(peer1, &rset, &wset) ) break;
        if ( action(peer1, &rset, &wset) ) break;
        printf("peer1: %d %d %d\n", peer1->sock, peer1->nread, peer1->nwrite);
        printf("peer2: %d %d %d\n", peer2->sock, peer2->nread, peer2->nwrite);
    };
    free ( peer1-> buf);
    free ( peer2-> buf);
};

void start(struct peer* peer1, struct peer* peer2) {
    printf("start\n");
    fcntl (peer1->sock, F_SETFL, O_NONBLOCK);
    fcntl (peer2->sock, F_SETFL, O_NONBLOCK);
    EINPROGRESS != (connect(peer1->sock, &peer1->remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer1");
    EINPROGRESS != (connect(peer2->sock, &peer2->remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer2");
    waitonconnect(peer1->sock,peer2->sock);
    printf("connected\n");
    run(peer1, peer2);
};

int main(int argc, char *argv[]) {

  struct peer peer1, peer2;

  if ( argc != 3 ) die("expecting just two arguments");

  if ( 0 != getPeer(argv[1],&peer1)) die("could not intialise peer for arg 1");
  if ( 0 != getPeer(argv[2],&peer2)) die("could not intialise peer for arg 2");
  start(&peer1, &peer2);
  printf("all done\n");
}
