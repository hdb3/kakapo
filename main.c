
/* kakapo - a BGP traffic source and sink */

#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/sendfile.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/time.h>
#include <pthread.h>

#include "sockbuf.h"
#include "util.h"
#include "session.h"
#include "kakapo.h"

#define MAXPENDING 5    // Max connection requests
#define BUFFSIZE 0x10000

#ifndef MYAS
#define MYAS 65001
#endif

int pid;
int tidx = 0;

char * fnOpen, * fnUpdate;

void startsession(int sock, int tid, char * fn1, char * fn2) {
  struct sockaddr_in peeraddr,myaddr;
  // get the local address that is being used for this session
  int socklen;
  ((0 == getsockname(sock,&myaddr,&socklen) && (socklen==SOCKADDRSZ)) || die ("Failed to find local address"));
  ((0 == getpeername(sock,&peeraddr,&socklen) && (socklen==SOCKADDRSZ)) || die ("Failed to find peer address"));
  fprintf(stderr, "%d: local address %s\n",pid, inet_ntoa(myaddr.sin_addr));
  fprintf(stderr, "%d: peer address %s\n",pid, inet_ntoa(peeraddr.sin_addr));

  struct sessiondata *sd;
  sd = malloc (sizeof(struct sessiondata));
  *sd = (struct sessiondata) { sock , tid, myaddr.sin_addr.s_addr, peeraddr.sin_addr.s_addr, MYAS, fnOpen , fnUpdate };

  pthread_t thrd;
  pthread_create(&thrd, NULL, session, sd);
};

void client (char *s) {
  int peersock;
  struct sockaddr_in peeraddr,myaddr;

  fprintf(stderr, "%d: Connecting to: %s\n",pid, s);

  // parse the parameter string for optional source address

  memset(&peeraddr, 0, SOCKADDRSZ );
  peeraddr.sin_family = AF_INET;
  peeraddr.sin_port = htons(179);


  memset(&myaddr, 0, SOCKADDRSZ );
  myaddr.sin_family = AF_INET;
  myaddr.sin_port = 0; // allow the OS to choose the outbound port (redundant as we did a memset)

  (0 < (peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP) ) || die ("Failed to create socket"));

  if (0 == inet_aton(s,&peeraddr.sin_addr)) { // failed to parse as a single address..
  // if there is no comma, it fails
  // if there is a comma then only if aton on both halves works is it ok...
    char * commaloc;
    if ( 0 != (commaloc = strchr (s,','))) {
        *commaloc = 0;
        if (inet_aton(s,&peeraddr.sin_addr) && inet_aton(commaloc+1,&myaddr.sin_addr)) {
            (0 == bind(peersock,&myaddr,SOCKADDRSZ) || die ("Failed to bind local address"));
            fprintf(stderr, "%d: success using local address %s\n",pid, inet_ntoa(myaddr.sin_addr));
        } else
            die ("Failed to parse address(es)");
    } else
        die ("Failed to parse address(es)");
  };
  fprintf(stderr, "%d: using peer address %s\n",pid, inet_ntoa(peeraddr.sin_addr));

  (0 == (connect(peersock, (struct sockaddr *) &peeraddr, SOCKADDRSZ) ) || die ("Failed to connect with peer"));

  fprintf(stderr, "%d: Peer connected: %s\n",pid, s);
  startsession(peersock , tidx++, fnOpen , fnUpdate);
};


void server() {
  int serversock, peersock;
  struct sockaddr_in peeraddr;

    memset(&peeraddr, 0, SOCKADDRSZ );
    peeraddr.sin_family = AF_INET;
    peeraddr.sin_addr.s_addr = htonl(INADDR_ANY);   // local server addr - wildcard - could be a specific interface
    peeraddr.sin_port = htons(179);       // BGP server port

    if ((serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
      die("Failed to create socket");
    }

    int reuse = 1;
    if (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char*)&reuse, sizeof(reuse)) < 0)
      die("Failed to set server socket option SO_REUSEADDR");

    if (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char*)&reuse, sizeof(reuse)) < 0)
      die("Failed to set server socket option SO_REUSEPORT");

    if (bind(serversock, (struct sockaddr *) &peeraddr, SOCKADDRSZ ) < 0) {
      die("Failed to bind the server socket");
    }

    if (listen(serversock, MAXPENDING) < 0) {
      die("Failed to listen on server socket");
    }

    while (1) {
      unsigned int addrsize;
      fprintf(stderr, "%d: waiting for connection\n",pid);
      if ((peersock = accept(serversock, (struct sockaddr *) &peeraddr, &addrsize )) < 0) {
        die("Failed to accept peer connection");
      }
      if ( addrsize != SOCKADDRSZ || AF_INET != peeraddr.sin_family) {
        die("bad sockaddr");
      }
      fprintf(stderr, "%d: Peer connected: %s\n",pid, inet_ntoa(peeraddr.sin_addr));

      startsession(peersock , tidx++, fnOpen , fnUpdate);
    }
};

int main(int argc, char *argv[]) {

  pid = getpid();
  fprintf(stderr, "%d: kakapo\n",pid);
  if (3 > argc) {
      fprintf(stderr, "USAGE: bgpc <open_message_file> <update_message_file> {IP address}\n");
      exit(1);
  }

  fnOpen = argv[1];
  fnUpdate = argv[2];

 (0 == access(fnOpen,R_OK) || die ("Failed to open BGP Open message file"));
 (0 == access(fnUpdate,R_OK) || die ("Failed to open BGP Update message file"));

  if (3 == argc) { // server mode.....
    server();
  } else { // client mode
     int argn;
     for (argn=4 ; argn <= argc ; argn++)
         client(argv[argn-1]);
  }
  while (1)
    sleep(100);
}
