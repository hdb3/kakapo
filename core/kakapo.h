
/* kakapo - a BGP traffic source and sink */

#ifndef VERBOSE
#define VERBOSE (0)
#endif

// compile time defines for version strings
#ifndef VERSION
#define VERSION "????"
#endif
#ifndef BUILDDATE
#define BUILDDATE "????"
#endif
#ifndef BRANCH
#define BRANCH "????"
#endif

#include "sockbuf.h"
#include "stats.h"
#define SOCKADDRSZ (sizeof(struct sockaddr_in))

#ifdef TRACE
extern FILE *tracefile;
#endif

extern char *BGP_EXIT_STATUS; // exit status for most bgp tests, e.g. rate test.  "OK" if clean.

extern FILE *tracefile;
extern int pid;
extern bool interrupted;
extern bool txerror;
extern int tflag;
extern uint32_t PEERMAXRETRIES;
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
extern uint32_t RATEWINDOW;
extern uint32_t TABLESIZE;
extern uint32_t MAXBURSTCOUNT;
extern uint32_t RATECOUNT;
extern uint32_t RATETIMELIMIT;
extern uint32_t NEXTHOP;
extern uint32_t CYCLECOUNT;
extern uint32_t CYCLEDELAY;
extern uint32_t REPEATDELAY;
extern uint32_t HOLDTIME;
extern uint32_t RATEBLOCKSIZE;
extern uint32_t MAXBLOCKINGFACTOR;
extern char *MODE;
extern char SENDFILENAME[];

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
  uint32_t remoteas;
  uint16_t port;
  struct sockbuf sb;
  slp_t slp;
};

char *show_peer(struct peer *p);

extern struct peer *peertable;
extern struct peer *listener;
extern struct peer *senders;
extern int peer_count;
extern int sender_count;

void session(void *x);
void establish(void *x);
double file_test(char *fname);
double single_peer_burst_test(uint32_t count);
double multi_peer_burst_test(uint32_t count);
int multi_peer_rate_test(uint32_t count, uint32_t window);
int single_peer_rate_test(uint32_t count, uint32_t window);
void multi_peer_func_test(uint32_t count);
void single_peer_func_test(uint32_t count);
double conditioning();
void notify_all();
void canary_all();
void keepalive_all();

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

struct rate_test_data {
  double elapsed, duration;
  uint64_t message_count, calculated_rate;
};

void log_rate_test_data(struct rate_test_data *log_data);
