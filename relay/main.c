
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
char *displaypeer(struct peer p);

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
  args_t args = commaparse(s);

  if (0 == *args.argv[0]) // null string in pos 1 valid, use 0.0.0.0, implies
                          // this is a listen only peer definition
    p->remote = 0;
  else
    p->remote = toHostAddress(args.argv[0]);

  if ((args.argc < 2) ||
      0 == *args.argv[1]) // null string in pos 2 valid, use 0.0.0.0, implies
                          // this is a default 0.0.0.0 listener
    p->local = 0;
  else
    p->local = toHostAddress(args.argv[1]);

  if ((args.argc < 3) || 0 == *args.argv[2]) // null string in pos 3 valid, use
                                             // 0, implies this is a dynamic ASN
    p->as = 0;
  else if (1 != sscanf(args.argv[2], "%d", &p->as))
    die("could not read an AS number");

  return p;
};

char *displaypeer(struct peer p) {
  static char rval[42];
  int ix;
  ix = snprintf(rval, 42, "%s , ", fromHostAddress(p.remote));
  snprintf(rval + ix, 42 - ix, "%s , %d", fromHostAddress(p.local), p.as);
  return rval;
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
