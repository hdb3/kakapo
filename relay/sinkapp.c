/* kakapo-sinkapp.c - an example application which consumes the libsink library function.
  Derived from relayapp, roughly functionally equivalent.
  Variations:
  1) only sink traffic, never forward or echo, except keepaive and open messages
  2) accept connections at all times, run sessions independently
*/

#include "librelay.h"
#include "util.h"
// below needed for SO_REUSEPORT
#include <asm-generic/socket.h>
#define FLAGS(a, b, c)

#define SOCKADDRSZ (sizeof(struct sockaddr_in))
#define MAXPENDING 100 // Max connection requests

extern void sink(struct peer *p);
extern void peer_report(struct peer *p);

struct peer peer_table[MAXPEERS];

void usage() {
  printf("sinkapp <<sink address>>\n");
}

/*
  process logic

  the application runs indefinitely

*/
int main(int argc, char *argv[]) {
  sigset_t set;

  struct in_addr sink_addr;
  int source_count = 1;
  struct sockaddr_in acceptaddr;
  int peersock;
  socklen_t socklen;
  struct peer *p;
  struct sockaddr_in host = {AF_INET, htons(179), (struct in_addr){0}};
  int serversock;
  int reuse;
  struct in_addr listener_addr;
  struct sockaddr_in remote, local;
  int sink_connected;
  int sources_connected;
  int peer_index = 0;

  setlinebuf(stdout);
  prctl(PR_SET_DUMPABLE, 1);
  sigemptyset(&set);
  sigaddset(&set, SIGPIPE);
  pthread_sigmask(SIG_BLOCK, &set, NULL);

  0 < (serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) || die("Failed to create socket");
  reuse = 1;
  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEADDR");
  reuse = 1;
  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEPORT");
  0 == (bind(serversock, (struct sockaddr *)&host, SOCKADDRSZ)) || die("Failed to bind the server socket");
  0 == (listen(serversock, MAXPENDING)) || die("Failed to listen on server socket");

  while (1) {

    memset(&acceptaddr, 0, SOCKADDRSZ);
    -1 != (peersock = accept(serversock, NULL, NULL)) || die("Failed to accept peer connection");

    socklen = SOCKADDRSZ;
    0 == (getpeername(peersock, (struct sockaddr *)&remote, &socklen)) || die("Failed to get peer address");
    socklen = SOCKADDRSZ;
    0 == (getsockname(peersock, (struct sockaddr *)&local, &socklen)) || die("Failed to get local address");
    printf("connected:");
    printf("local: %s ", inet_ntoa(local.sin_addr));
    printf("remote: %s\n", inet_ntoa(remote.sin_addr));

    p = &peer_table[peer_index++];
    memset(p, 0, sizeof(struct peer));
    p->sock = peersock;
    p->local = local;
    p->remote = remote;
    sink(p);
    peer_report(p);
  };
};
