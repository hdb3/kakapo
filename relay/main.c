
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

int waitonconnect (int fd1, int fd2 ) {
  fd_set set, tset;
  struct timeval timeout = {1,0};
  struct timeval notime = {0,0};

  FD_ZERO (&set);
  FD_SET (fd1, &set);
  FD_SET (fd2, &set);

  tset = set;
  while ( 2 > select (FD_SETSIZE, NULL, &tset, NULL, &notime)) {
      if ( FD_ISSET ( fd1 , &tset)) {
          tset = set;
          FD_CLR ( fd1 , &tset);
      } else if ( FD_ISSET ( fd2 , &tset)) {
          tset = set;
          FD_CLR ( fd2 , &set);
      }
      select (FD_SETSIZE, NULL, &tset, NULL, &timeout);
      tset = set;
  };
  int err,szerr;
  szerr = sizeof(err) ; getsockopt(fd1, SOL_SOCKET, SO_ERROR, (void *)&err, &szerr);
  if (szerr != sizeof(err) || err != 0) {
    printf("connect error on fd1 (%d)\n",err);
    return -1;
  };
  szerr = sizeof(err) ; getsockopt(fd1, SOL_SOCKET, SO_ERROR, (void *)&err, &szerr);
  if (szerr != sizeof(err) || err != 0) {
    printf("connect error on fd2 (%d)\n",err);
    return -1;
  };
  return 0;
};

void showselectflags ( char* ctxt, fd_set* rset, fd_set* wset, int fd1, int fd2 ) {
    printf("select flags: %s  peer1: [ ", ctxt);
    if ( FD_ISSET ( fd1 , rset )) printf("READ ");
    if ( FD_ISSET ( fd1 , wset )) printf("WRITE ");
    printf(" ]\n");
    printf("select flags: %s  peer2: [ ", ctxt);
    if ( FD_ISSET ( fd2 , rset )) printf("READ ");
    if ( FD_ISSET ( fd2 , wset )) printf("WRITE ");
    printf(" ]\n");
};

int action (struct peer* p, struct peer* p2, fd_set* rset, fd_set* wset) {
    int res;
    if ( p->nread ==0 ) {
    // we are in read mode
        if ( FD_ISSET ( p->sock , rset)) {
            res = read(p->sock, p->buf, BUFSIZE);
            if ( res > 0 ) {
                p->nread = res;
                p->nwrite = 0;
                FD_SET ( p2->sock , wset); // set this flag so that the immediate write can happen
                // printf("read success on fd%d\n", p->sock);
            } else if ( res = 0 )  // normal end-of-stream
                return 1;
            else if ( errno == EAGAIN )  // nothing available but not an error
                printf("got nothing from a read, just saying....\n");
            else {
                printf("end of stream, errno: %d\n",errno);
                return 1;
            };
        };
    };

    if ( p->nread > 0 ) {
    // we are in write mode
        if ( FD_ISSET ( p2->sock , wset)) {
            res = write(p2->sock, p->buf+p->nwrite, p->nread - p->nwrite);
            if ( res > 0 ) {
                p->nwrite += res;
                // printf("write success on fd%d\n", p->sock);
            } else
                die ("unexpected condition in action/read mode");
            if ( p->nwrite = p->nread )
                p->nread = 0; // signal change of mode
                p->nwrite = 0;
        };
    };

    if (p->nread) {
    // we are now/still in write mode
        FD_SET ( p2->sock , wset );
        FD_CLR ( p->sock , rset );
    } else {
    // we are now in read mode
        FD_SET ( p->sock , rset );
        FD_CLR ( p2->sock , wset );
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
        //showselectflags("after select", &rset, &wset, peer1->sock, peer2->sock);
        //printf("select yielded %d\n", res);
        if ( action(peer1, peer2, &rset, &wset) ) break;
        if ( action(peer2, peer1, &rset, &wset) ) break;
        //printf("peer1: %d %d %d\n", peer1->sock, peer1->nread, peer1->nwrite);
        //printf("peer2: %d %d %d\n", peer2->sock, peer2->nread, peer2->nwrite);
        //showselectflags("after action", &rset, &wset, peer1->sock, peer2->sock);
    };
    free ( peer1-> buf);
    free ( peer2-> buf);
};

int start(char* p1, char* p2) {
    int i;
    struct peer peer1, peer2;

    printf("start\n");
    if ( 0 != getPeer(p1,&peer1)) die("could not intialise peer for arg 1");
    if ( 0 != getPeer(p2,&peer2)) die("could not intialise peer for arg 2");
    fcntl (peer1.sock, F_SETFL, O_NONBLOCK);
    fcntl (peer2.sock, F_SETFL, O_NONBLOCK);
    i=1; setsockopt(peer1.sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
    i=1; setsockopt(peer2.sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
    EINPROGRESS != (connect(peer1.sock, &peer1.remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer1");
    EINPROGRESS != (connect(peer2.sock, &peer2.remote, SOCKADDRSZ)) ||
      die("Failed to start connect with peer2");
    int res = waitonconnect(peer1.sock,peer2.sock);
    if (0 == res) {
        printf("connected\n");
        run(&peer1, &peer2);
    } else {
        printf("rejected\n");
    };
    close (peer1.sock);
    close (peer2.sock);
    return res;
};

int main(int argc, char *argv[]) {

  if ( argc != 3 ) die("expecting just two arguments");

  while (1) {
    while (0 != start(argv[1],argv[2])) {
        printf("retrying\n");
        sleep(3);
    };
  };
  printf("all done\n");
}
