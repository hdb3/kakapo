
/* kakapo - a BGP traffic source and sink */

#ifndef VERBOSE
#define VERBOSE (0)
#endif

#define SOCKADDRSZ (sizeof(struct sockaddr_in))

extern int pid;
extern int tflag;
extern uint32_t SHOWRATE;
extern uint32_t SLEEP;
extern uint32_t TIMEOUT;
extern uint32_t FASTCYCLELIMIT;
extern uint32_t IDLETHR;

extern uint32_t SEEDPREFIX;
extern uint32_t SEEDPREFIXLEN;
extern uint32_t GROUPSIZE;
extern uint32_t BLOCKSIZE;
extern uint32_t MAXBURSTCOUNT;
extern uint32_t NEXTHOP;
extern uint32_t CYCLECOUNT;
extern uint32_t CYCLEDELAY;
extern uint32_t HOLDTIME;

void startlog(uint32_t tid, char *tids, struct timespec *start);
// void startlog(uint32_t tid,char *tids, struct timespec *start,uint32_t
// BLOCKSIZE, uint32_t GROUPSIZE, uint32_t MAXBURSTCOUNT, uint32_t CYCLECOUNT,
// uint32_t CYCLEDELAY);
void endlog(char *s);
void sndlog(uint32_t tid, char *tids, uint32_t seq, struct timespec *start,
            struct timespec *end);
void rcvlog(uint32_t tid, char *tids, uint32_t seq, struct timespec *start,
            struct timespec *end);
uint32_t senderwait();
void receiversignal(uint32_t seq);
extern struct timespec txts;

#define ROLELISTENER 1
#define ROLESENDER 2
struct peer {
  int sock;
  int tidx;
  int role;
  pthread_t thrd;
  uint32_t remote;
  uint32_t local;
  uint32_t as;
};

//*void session(struct sessiondata *sd);
void *session(void *x);

#define LIMIT 3

void parseargument(struct peer *p, char *s);
char *displaypeer(struct peer *p);

struct args {
  int argc;
  char **argv;
};

typedef struct args args_t;
args_t commaparse(char *s);
