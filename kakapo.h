
/* kakapo - a BGP traffic source and sink */

#ifndef VERBOSE
#define VERBOSE (0)
#endif

#define _GNU_SOURCE
#ifndef VERSION
#define VERSION "version undefined"
#endif
#ifndef BUILDDATE
#define BUILDDATE "builddate unknown"
#endif
#include "sockbuf.h"
#include "stats.h"
#define SOCKADDRSZ (sizeof(struct sockaddr_in))

extern int pid;
extern int tflag;
extern uint32_t TCPPORT;
extern uint32_t SHOWRATE;
extern uint32_t SLEEP;
extern uint32_t TIMEOUT;
extern uint32_t FASTCYCLELIMIT;
extern uint32_t REPEAT;
extern uint32_t IDLETHR;

extern uint32_t CANARYSEED;
extern uint32_t SEEDPREFIX;
extern uint32_t SEEDPREFIXLEN;
extern uint32_t GROUPSIZE;
extern uint32_t WINDOW;
extern uint32_t BLOCKSIZE;
extern uint32_t TABLESIZE;
extern uint32_t MAXBURSTCOUNT;
extern uint32_t NEXTHOP;
extern uint32_t CYCLECOUNT;
extern uint32_t CYCLEDELAY;
extern uint32_t HOLDTIME;
extern char *MODE;

void startlog(uint32_t tid, char *tids, struct timespec *start);
void endlog(char *s);
void sndlog(uint32_t tid, char *tids, uint32_t seq, struct timespec *start, struct timespec *end);
void rcvlog(uint32_t tid, char *tids, uint32_t seq, struct timespec *start, struct timespec *end);
uint32_t senderwait();
void receiversignal(uint32_t seq);
extern struct timespec txts;

#define ROLELISTENER 1
#define ROLESENDER 2
struct peer {
  int sock;
  int tidx;
  int role;
  int sndrunning;
  pthread_t thrd;
  int sendFlag;
  uint32_t remoteip;
  uint32_t localip;
  uint32_t as;
  struct sockbuf sb;
  slp_t slp;
};

void *session(void *x);
void *establish(void *x);
double single_peer_burst_test(struct peer *p, int count);
double multi_peer_burst_test(struct peer *p, int count);
void multi_peer_rate_test(struct peer *p, int count, int window);
void conditioning(struct peer *p);
void notify_all(struct peer *p);
void strict_canary_all(struct peer *p);

#define LIMIT 3

void parseargument(struct peer *p, char *s);
char *displaypeer(struct peer *p);

struct args {
  int argc;
  char **argv;
};

typedef struct args args_t;
args_t commaparse(char *s);

#define BGPENDOFSTREAM (-1)
#define BGPTIMEOUT 0
#define BGPOPEN 1
#define BGPUPDATE 2
#define BGPNOTIFICATION 3
#define BGPKEEPALIVE 4
#define BGPUNKNOWN 5

struct bgp_message {
  char *payload;
  uint16_t pl;
  unsigned char msgtype;
};
