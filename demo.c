
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#define MAXPENDING 5    // Max connection requests
#define SOCKADDRSIZE (sizeof (struct sockaddr_in))

int main (int argc, char** argv) {

  socklen_t addrsize;
  int serversock,
      peersock,
      l;
  struct sockaddr_in sockaddr;

  sockaddr.sin_family = AF_INET;
  sockaddr.sin_addr.s_addr = htonl(INADDR_ANY);   // local server addr - wildcard
  sockaddr.sin_port = htons(179);       // BGP server port

  serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  bind(serversock, &sockaddr, SOCKADDRSIZE);
  printf("\nlistening\n");
  listen(serversock, MAXPENDING);

  while (1) {
    // without the following statement the return value appears to show the wildcard address
    //  (actually it is simply unmodified memory which may happen to be zeros)
    addrsize = SOCKADDRSIZE;
    peersock = accept(serversock, &sockaddr, &addrsize );
    printf("\nconnected\n");
    printf("accept address %s\n", inet_ntoa(sockaddr.sin_addr));

    addrsize = SOCKADDRSIZE;
    getsockname(peersock,&sockaddr,&addrsize);
    printf("local address %s\n", inet_ntoa(sockaddr.sin_addr));

    addrsize = SOCKADDRSIZE;
    getpeername(peersock,&sockaddr,&addrsize);
    printf("peer address %s\n", inet_ntoa(sockaddr.sin_addr));
    do {
        char s [20];
        l = recv(peersock,s,20,0);
    } while (l>0);
    close(peersock);
  };
};
