#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>

#define MAXPEERS 100
struct peer {
  // TODO remove peer_index, use an index as the peer refernces ratther than a pointer
  int active, peer_index, sock;
  uint64_t nread, nwrite, nprocessed, nwriteable;
  int msg_counts[5];
  struct sockaddr_in remote, local;
  void *buf;
};

extern void relay(int peer_count, struct peer *peer_table);
void peer_reports(int peer_count, struct peer *peer_table);
