/* kakapo-relayapp.c - an example application which consumes the librelay library function.
   in 'kakapo-librelay - a BGP traffic relay library'
  Derived from relay2, roughly functionally equivalent.
  Variations:
  1) predetermined number of source peers - command line is number if greater than 1, and the listener.
  2) ......

  Note: server listens on default port 179 and on all ipv4 interface addreses (0.0.0.0)
*/

#include "librelay.h"
#include "util.h"
// below needed for SO_REUSEPORT
#include <asm-generic/socket.h>
#define FLAGS(a, b, c)

#define SOCKADDRSZ (sizeof(struct sockaddr_in))
#define MAXPENDING 100 // Max connection requests

struct peer peer_table[MAXPEERS];

void usage() {
  printf("relayapp <<sink address>> [source peer count]\n");
}

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
  int sink_connected = 0;
  int sources_connected = 0;
  int peer_index = -1;

  setlinebuf(stdout);
  prctl(PR_SET_DUMPABLE, 1);
  sigemptyset(&set);
  sigaddset(&set, SIGPIPE);
  pthread_sigmask(SIG_BLOCK, &set, NULL);

  if ((argc == 1) || (argc > 3)) {
    usage();
  } else {
    0 != (inet_aton(argv[1], &sink_addr)) || die("failed parsing sink peer address");
    if (argc == 3) {
      char *tail;
      source_count = strtoul(argv[2], &tail, 0);
      tail > argv[2] || die("failed parsing source peer count");
      ((source_count > 0) && (source_count < MAXPEERS)) || die("invalid source peer count");
    }

    printf("server start\n");
    printf("sink is: %s, source peer count is: %d\n", inet_ntoa(sink_addr), source_count);

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

      if (remote.sin_addr.s_addr == sink_addr.s_addr) {
        sink_connected = 1;
        peer_index = 0;
      } else {
        sources_connected += 1;
        peer_index = sources_connected;
      };

      p = &peer_table[peer_index];
      memset(p, 0, sizeof(struct peer));
      p->sock = peersock;

      if (sink_connected && (source_count <= sources_connected)) {
        printf("peers connected: start sessions\n");
        break;
      } else {
        printf("sink peer: %s, ", sink_connected ? "connected" : "waiting");
        printf("source peers connected: %d/%d\n", sources_connected, source_count);
      }
    };
    printf("starting librelay\n");
    relay(sources_connected + 1, peer_table);
    printf("librelay exited\n");
    peer_reports(sources_connected + 1, peer_table);
  };
};
