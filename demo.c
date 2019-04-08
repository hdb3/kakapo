

#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

#define MAXPENDING 5    // Max connection requests

int main (int argc, char** argv) {

  socklen_t addrsize;
  int serversock,
      peersock;
  struct sockaddr_in server_addr,
                    local_addr,
                    peer_addr,
                    accept_addr;

  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = htonl(INADDR_ANY);   // local server addr - wildcard
  server_addr.sin_port = htons(179);       // BGP server port

  serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  bind(serversock, (struct sockaddr *) &server_addr, sizeof (struct sockaddr_in) );
  printf("\nlistening\n");
  listen(serversock, MAXPENDING);

  while (1) {
    memset(&accept_addr, 0, (sizeof (struct sockaddr_in)) );
    peersock = accept(serversock, (struct sockaddr *) &accept_addr, &addrsize );
    printf("\nconnected\n");
    printf("accept address %s\n", inet_ntoa(accept_addr.sin_addr));

    memset(&local_addr, 0, (sizeof (struct sockaddr_in)) );
    getsockname(peersock,&local_addr,&addrsize);
    printf("local address %s\n", inet_ntoa(local_addr.sin_addr));

    memset(&peer_addr, 0, (sizeof (struct sockaddr_in)) );
    getpeername(peersock,&peer_addr,&addrsize);
    printf("peer address %s\n", inet_ntoa(peer_addr.sin_addr));

    close(peersock);
  };
};
