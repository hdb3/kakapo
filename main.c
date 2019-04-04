
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
#define SOCKADDRSZ (sizeof (struct sockaddr_in))

int pid;

int main(int argc, char *argv[]) {
  int serversock, peersock, fd1,fd2;
  struct sockaddr_in peeraddr;

  pid = getpid();
  fprintf(stderr, "%d: kakapo\n",pid);
  if (3 > argc) {
      fprintf(stderr, "USAGE: bgpc <open_message_file> <update_message_file> {IP address}\n");
      exit(1);
  }

 (0 == access(argv[1],R_OK) || die ("Failed to open BGP Open message file"));
 (0 == access(argv[2],R_OK) || die ("Failed to open BGP Update message file"));
 
  if (3 == argc) { // server mode.....

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

      //*void session(void *x);
      //session(peersock,argv[1],argv[2]);
      struct sessiondata sd = { peersock , argv[1] , argv[2] };
      (int*) session( (void *) &sd);
      //pthread_t thrd;
      //pthread_create(&thrd, NULL, session, &sd);
    }
  } else { // client mode
      fprintf(stderr, "%d: Connecting to: %s\n",pid, argv[3]);
      memset(&peeraddr, 0, SOCKADDRSZ );
      peeraddr.sin_family = AF_INET;
      peeraddr.sin_addr.s_addr = inet_addr(argv[3]);
      peeraddr.sin_port = htons(179);
      if ((peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
        die("Failed to create socket");
      } else if (connect(peersock, (struct sockaddr *) &peeraddr, SOCKADDRSZ ) < 0) {
        die("Failed to connect with peer");
      } else {
          fprintf(stderr, "%d: Peer connected: %s\n",pid, argv[3]);
          struct sessiondata sd = { peersock , argv[1] , argv[2] };
          (int*) session( (void *) &sd);
          //session(peersock,argv[1],argv[2]);
      }
  }
}
