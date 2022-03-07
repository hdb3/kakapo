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
#define FLAGS(a, b, c)

#define SOCKADDRSZ (sizeof(struct sockaddr_in))
#define BUFSIZE (1024 * 1024 * 64)
#define MINREAD 4096
#define MAXPEERS 100
#define MAXPENDING 100 // Max connection requests

char VERSION[] = "2.0.0";

struct peer peer_table[MAXPEERS];
// int peer_count = 0;
// int nfds = 0;
// int running = 0;
// int listen_sock = -1;

void showpeer(struct peer *p) {
  printf("peer %2d: local: %s ", p->peer_index, inet_ntoa(p->local.sin_addr));
  printf("         remote: %s\n", inet_ntoa(p->remote.sin_addr));
};

void peer_report(struct peer *p) {
  printf("\npeer report\n");
  showpeer(p);
  printf("%ld/%ld/%ld bytes read/written/processed\n", p->nread, p->nwrite, p->nprocessed);
  printf("%d messages\n", *(p->msg_counts));
  printf("%d Opens\n", (p->msg_counts)[1]);
  printf("%d Updates\n", (p->msg_counts)[2]);
  printf("%d Notifications\n", (p->msg_counts)[3]);
  printf("%d Keepalives\n", (p->msg_counts)[4]);
};

// void setsocketnonblock(int sock) {
//   fcntl(sock, F_SETFL, O_NONBLOCK);
// };

// void setsocketnodelay(int sock) {
//   int i = 1;
//   setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
// };

void server(struct in_addr sink, int source_count) {
  struct sockaddr_in acceptaddr;
  int peersock;
  socklen_t socklen;
  struct peer *p;
  struct sockaddr_in host = {AF_INET, htons(179), (struct in_addr){0}};
  int serversock;
  int reuse;
  struct in_addr listener_addr;
  pthread_t threadid;
  struct sockaddr_in remote, local;
  int sink_connected = 0;
  int sources_connected = 0;
  int peer_index = -1;

  printf("server start\n");
  printf("sink is: %s, source peer count is: %d\n", inet_ntoa(sink), source_count);

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

    if (remote.sin_addr.s_addr == sink.s_addr) {
      sink_connected = 1;
      peer_index = 0;
    } else {
      sources_connected += 1;
      peer_index = sources_connected;
    };

    // fcntl(peersock, F_SETFL, O_NONBLOCK);
    // setsocketnonblock(peersock);
    p = &peer_table[peer_index];
    memset(p, 0, sizeof(struct peer));
    p->peer_index = peer_index;
    p->active = 1;
    p->buf = malloc(BUFSIZE);
    nfds = peersock + 1 > nfds ? peersock + 1 : nfds;
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
};

void usage() {
  printf("relayapp <<sink address>> [source peer count]\n");
}

int main(int argc, char *argv[]) {
  sigset_t set;

  setlinebuf(stdout);
  prctl(PR_SET_DUMPABLE, 1);
  sigemptyset(&set);
  sigaddset(&set, SIGPIPE);
  pthread_sigmask(SIG_BLOCK, &set, NULL);
  struct in_addr sink_addr;
  int source_count = 1;

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
    server(sink_addr, source_count);
  };
};
