
/* kakapo - a BGP traffic source and sink */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include <stdint.h>

#include "util.h"


#define SOCKADDRSZ (sizeof(struct sockaddr_in))

struct peer {
  uint32_t remote;
  uint32_t local;
  uint32_t as;
};

struct args {
  int argc;
  char **argv;
};
typedef struct args args_t;
args_t commaparse(char *s);


// ################################

// number of fields we will collect in a single argument
#define LIMIT 3 

args_t commaparse(char *s) {
  static char *argv[3];
  int argc = 1;
  argv[0] = s;
  while ((0 != *s) && (argc < LIMIT)) {
    if (',' == *s) {
      *s = 0;
      argv[argc++] = s + 1;
    };
    s++;
  };
  return (args_t){argc, argv};
};

struct peer *parseargument(char *s) {
  struct peer *p = malloc(sizeof(struct peer));
  return p;
};

// ################################

void *clientthread(void *_p) {
  struct peer *p = (struct peer *)_p;
  int peersock;
  struct sockaddr_in peeraddr = {AF_INET, htons(179), (struct in_addr){p->remote}};
  struct sockaddr_in myaddr = {AF_INET, 0, (struct in_addr){p->local}};

  0 < (peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) ||
      die("Failed to create socket");

  0 == bind(peersock, &myaddr, SOCKADDRSZ) ||
      die("Failed to bind local address");

  0 == (connect(peersock, &peeraddr, SOCKADDRSZ)) ||
      die("Failed to connect with peer");

};

void peer(char *s) {
  struct peer *p = parseargument(s);
  pthread_t thrd;
  if (0 == p->remote) // servers have a zero 'remote' address
    ; // pthread_create(&thrd, NULL, serverthread, (void *)p);
  else
    pthread_create(&thrd, NULL, clientthread, (void *)p);
};
int main(int argc, char *argv[]) {

  int argn;
  for (argn = 1; argn <= argc - 1; argn++)
    peer(argv[argn]);
  while (1)
    sleep(100);
}
