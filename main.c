
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

#include "kakapo.h"
#include "parsearg.h"
#include "session.h"
#include "sockbuf.h"
#include "stats.h"
#include "util.h"

#define MAXPENDING 5 // Max connection requests

sem_t semrxtx;
struct timespec txts;

int pid;
int tidx = 0;
uint32_t SLEEP = 0; // default value -> don't repeat the send operation
uint32_t TIMEOUT = 10;

uint32_t SHOWRATE = 0;
uint32_t SEEDPREFIXLEN = 30;
uint32_t GROUPSIZE = 3;
uint32_t BLOCKSIZE = 3;
uint32_t MAXBURSTCOUNT = 3;
uint32_t NEXTHOP;
char sNEXTHOP[] = "192.168.1.1"; // = toHostAddress("192.168.1.1");  /// cant
                                 // initilase like this ;-(
uint32_t SEEDPREFIX;
char sSEEDPREFIX[] = "10.0.0.0"; // = toHostAddress("10.0.0.0");  /// cant
                                 // initilase like this ;-(
uint32_t CYCLECOUNT =
    1;                    // 0 => continuous, use MAXBURSTCOUNT = 0 to suppress sending at all
uint32_t CYCLEDELAY = 30; // seconds
uint32_t HOLDTIME = 180;

char LOGFILE[128] = "stats.csv";
char LOGPATH[128] = "localhost";
char LOGTEXT[1024] = "";
char ROLE[128] = "DUALMODE"; // only LISTENER and SENDER have any effect
char LISTENER[] = "LISTENER";
char SENDER[] = "SENDER";
int isListener() { return strncmp(LISTENER, ROLE, 9); };
int isSender() { return strncmp(SENDER, ROLE, 7); };
uint32_t IDLETHR = 1; // 1 seconds default burst idle threshold

void startsession(int sock, int as) {

  struct sessiondata *sd;
  sd = malloc(sizeof(struct sessiondata));
  *sd = (struct sessiondata){sock, tidx++, as};
  if (0 == isListener()) {
    sd->role = ROLELISTENER;
    fprintf(stderr, "%d: ROLE=LISTENER FROM ENVIRONMENT\n", pid);
  } else if (0 == isSender()) {
    sd->role = ROLESENDER;
    fprintf(stderr, "%d: ROLE=SENDER FROM ENVIRONMENT\n", pid);
  } else if (1 == tidx)
    sd->role = ROLELISTENER;
  else if (2 == tidx)
    sd->role = ROLESENDER;
  pthread_t thrd;
  pthread_create(&thrd, NULL, session, sd);
};

//void client(struct peer *p) {
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

  startsession(peersock, p->as);
};

//void * serverthread (struct peer *p) {
void *serverthread(void *_p) {
  struct peer *p = (struct peer *)_p;
  struct sockaddr_in acceptaddr;
  int peersock;
  socklen_t socklen;
  long int serversock;
  int reuse = 1;
  struct sockaddr_in hostaddr = {AF_INET, htons(179), (struct in_addr){p->local}};

  0 < (serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) || die("Failed to create socket");

  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEADDR");

  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char *)&reuse, sizeof(reuse))) || die("Failed to set server socket option SO_REUSEPORT");

  fprintf(stderr,"binding to %s\n",fromHostAddress(p->local));

  0 == (bind(serversock, &hostaddr, SOCKADDRSZ)) || die("Failed to bind the server socket");

  0 == (listen(serversock, MAXPENDING)) || die("Failed to listen on server socket");

  while (1) {
    memset(&acceptaddr, 0, SOCKADDRSZ);
    socklen = SOCKADDRSZ;
    0 < (peersock = accept((long int)serversock, &acceptaddr, &socklen)) || die("Failed to accept peer connection");
    (SOCKADDRSZ == socklen && AF_INET == acceptaddr.sin_family) || die("bad sockaddr");
    startsession(peersock, p->as);
  }
};

void peer(char *s) {
  struct peer *p = parseargument(s);
  pthread_t thrd;
  if (0 == p->remote) // servers have a zero 'remote' address
    pthread_create(&thrd, NULL, serverthread, (void *)p);
  else
    pthread_create(&thrd, NULL, clientthread, (void *)p);
};

// NOTE - the target string must be actual static memory large enough...
void getsenv(char *name, char *tgt) {
  char *s;
  if (s = getenv(name)) {
    strcpy(tgt, s);
    fprintf(stderr, "%d: read %s from environment: %s\n", pid, name, s);
  };
};

void gethostaddress(char *name, uint32_t *tgt) {
  char *s;
  if ((s = getenv(name)) && (1 == sscanf(s, "%s", s))) {
    *tgt = toHostAddress(s);
    fprintf(stderr, "%d: read %s from environment: %s\n", pid, name, fromHostAddress(*tgt));
  };
};

void getuint32env(char *name, uint32_t *tgt) {
  char *s;
  uint32_t n;
  if ((s = getenv(name)) && (1 == sscanf(s, "%d", &n))) {
    *tgt = n;
    fprintf(stderr, "%d: read %s from environment: %d\n", pid, name, n);
  };
};

void getllienv(char *name, long long int *tgt) {
  char *s;
  long long int n;
  if ((s = getenv(name)) && (1 == sscanf(s, "%lld", &n))) {
    *tgt = n;
    fprintf(stderr, "%d: read %s from environment: %lld\n", pid, name, n);
  };
};

FILE *logfile;
void endlog() {
  char *sp;
  fprintf(logfile, "HDR , STOP\nSTOP,%s\n", shownow());
  fclose(logfile);
  if (0 != LOGPATH) {
    time_t t = time(NULL);
    int tmp = asprintf(&sp,"curl -X PUT --data-binary @%s http://%s/%ld",LOGFILE,LOGPATH,t);
    fprintf(stderr,"trying to send datafile with: %s\n",sp);
    int res = system(sp);
    if (0 == res)
      fprintf(stderr,"success\n");
    else
      fprintf(stderr,"fail(%d)\n",res);
    free(sp);
  };
  exit(0);
};

void startlog(uint32_t tid, char *tids, struct timespec *start) {
  0 != (logfile = fopen(LOGFILE, "w")) || die("could not open log file");
  setvbuf(logfile, NULL, _IOLBF, 0);

  fprintf(stderr, "\n%s startlog at %s BLOCKSIZE %d, GROUPSIZE %d, MAXBURSTCOUNT %d, CYCLECOUNT %d, CYCLEDELAY %d\n",
          tids, showtime(start), BLOCKSIZE, GROUPSIZE, MAXBURSTCOUNT, CYCLECOUNT, CYCLEDELAY);

  fprintf(logfile,
          "HDR , PID , DESC , START , BLOCKSIZE, GROUPSIZE, MAXBURSTCOUNT, CYCLECOUNT, CYCLEDELAY\n"
          "START, %d, \"%s\" , \"%s\" , %d, %d, %d, %d, %d\n"
          "HDR , SEQ , RTT , LATENCY , TXDURATION, RXDURATION\n",
          pid, LOGTEXT, showtime(start), BLOCKSIZE, GROUPSIZE, MAXBURSTCOUNT,
          CYCLECOUNT, CYCLEDELAY);
};

struct timespec sndlog_start, sndlog_end;
uint32_t sndlog_seq;
void sndlog(uint32_t tid, char *tids, uint32_t seq, struct timespec *start,
            struct timespec *end) {
  // this is an immediate print, however the coordination with rcv is central,
  // so best left till rcvlog if generating readable output (the sequence nubers
  // can be used to correlate) note also: the caller context  (tid) is
  // implicitly igonred too. this may need rethinking for multiple senders...
  // // fprintf(stderr, "%s sndlog seq %d duration %f\n", tids, seq,
  // //         timespec_to_double(timespec_sub(*end, *start)));
  sndlog_seq = seq;
  memcpy(&sndlog_start, start, sizeof(struct timespec));
  memcpy(&sndlog_end, end, sizeof(struct timespec));
};

void rcvlog(uint32_t tid, char *tids, uint32_t seq, struct timespec *start,
            struct timespec *end) {
  // this is the corresponding freestanding report
  // fprintf(stderr, "%s rcvlog seq %d duration %f latency %f\n", tids, seq,
  //         timespec_to_double(timespec_sub(*end, *start)),
  //         timespec_to_double(timespec_sub(*end, txts)));
  assert(seq == sndlog_seq);
#ifndef RAWDATA
  fprintf(
      stderr,
      "burstlog seq %d rtt %f latency %f send duration %f recv duration %f\n",
      seq, timespec_to_double(timespec_sub(*end, sndlog_start)),
      timespec_to_double(timespec_sub(*start, sndlog_end)),
      timespec_to_double(timespec_sub(sndlog_end, sndlog_start)),
      timespec_to_double(timespec_sub(*end, *start)));
  fprintf(logfile, "DATA, %d , %f , %f , %f , %f\n", seq,
          timespec_to_double(timespec_sub(*end, sndlog_start)),
          timespec_to_double(timespec_sub(*start, sndlog_end)),
          timespec_to_double(timespec_sub(sndlog_end, sndlog_start)),
          timespec_to_double(timespec_sub(*end, *start)));
#else
  fprintf(
      stderr,
      "burstlog rawdate seq %d tx_start %f tx_end %f rx_start %f rx_end %f\n",
      seq, timespec_to_double(sndlog_start), timespec_to_double(sndlog_end),
      timespec_to_double(*start), timespec_to_double(*end));
  fprintf(logfile, "%d , %f , %f , %f , %f\n", seq,
          timespec_to_double(sndlog_start), timespec_to_double(sndlog_end),
          timespec_to_double(*start), timespec_to_double(*end));
#endif
};

uint32_t rcvseq;

void receiversignal(uint32_t seq) {
  rcvseq = seq;
  0 == (sem_post(&semrxtx)) || die("semaphore post fail");
};

uint32_t senderwait() {
  0 == (sem_wait(&semrxtx)) || die("semaphore wait fail");
  gettime(&txts);
  return rcvseq;
};

int main(int argc, char *argv[]) {

  pid = getpid();
  fprintf(stderr, "%d: kakapo\n", pid);
  if (1 > argc) {
    fprintf(stderr, "USAGE: kakapo {IP address[,IP address} [{IP address[,IP address}]\n");
    fprintf(stderr, "       many options are controlled via environment variables like SLEEP, etc...\n");
    exit(1);
  }

  0 == (sem_init(&semrxtx, 0, 0)) || die("semaphore create fail");
  NEXTHOP = toHostAddress(sNEXTHOP);       /// must initliase here because cant do it in the declaration
  SEEDPREFIX = toHostAddress(sSEEDPREFIX); /// cant initilase like this ;-(
  getuint32env("SLEEP", &SLEEP);
  getuint32env("TIMEOUT", &TIMEOUT);
  getuint32env("IDLETHR", &IDLETHR);
  gethostaddress("SEEDPREFIX", &SEEDPREFIX);
  getuint32env("SEEDPREFIXLEN", &SEEDPREFIXLEN);
  getuint32env("GROUPSIZE", &GROUPSIZE);
  getuint32env("BLOCKSIZE", &BLOCKSIZE);
  getuint32env("MAXBURSTCOUNT", &MAXBURSTCOUNT);
  gethostaddress("NEXTHOP", &NEXTHOP);
  getuint32env("CYCLECOUNT", &CYCLECOUNT);
  getuint32env("CYCLEDELAY", &CYCLEDELAY);
  getuint32env("SHOWRATE", &SHOWRATE);
  getuint32env("HOLDTIME", &HOLDTIME);
  getsenv("LOGFILE", LOGFILE);
  getsenv("LOGPATH", LOGPATH);
  getsenv("LOGTEXT", LOGTEXT);
  getsenv("ROLE", ROLE);

  startstatsrunner();
  int argn;
  for (argn = 1; argn <= argc - 1; argn++)
    peer(argv[argn]);
  while (1)
    sleep(100);
}
