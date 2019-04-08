
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

#include "util.h"

#define SOCKADDRSZ (sizeof (struct sockaddr_in))
// need to make this a library function so we can reuse it....

void session(int sock, int argc, char *argv[]);

int main(int argc, char *argv[]) {

  fprintf(stderr, "client\n");
  if (1 > argc) {
      fprintf(stderr, "USAGE: client {IP address}\n");
      exit(1);
  };

  char *s = argv[1];
  int peersock;
  struct sockaddr_in peeraddr,myaddr;

  fprintf(stderr, "Connecting to: %s\n",s);

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
            fprintf(stderr, "success using local address %s\n",inet_ntoa(myaddr.sin_addr));
        } else
            die ("Failed to parse address(es)");
    } else
        die ("Failed to parse address(es)");
  };
  fprintf(stderr, "using peer address %s\n", inet_ntoa(peeraddr.sin_addr));

  (0 == (connect(peersock, (struct sockaddr *) &peeraddr, SOCKADDRSZ) ) || die ("Failed to connect with peer"));

  fprintf(stderr, "Peer connected: %s\n", s);
  session(peersock,argc-2,argv+2);
};
