
/* kakapo - a BGP traffic source and sink */

#define _GNU_SOURCE
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
#include "sockbuf.h"
#include "stats.h"
//#include "util.h"

#define MAXPENDING 5 // Max connection requests

sem_t semrxtx;
struct timespec txts;

int pid;
int tflag = 0;      // the global termination flag - when != 0, exit gracefully
uint32_t SLEEP = 0; // default value -> don't rate limit the send operation
uint32_t TIMEOUT = 10;
uint32_t FASTCYCLELIMIT = 0; // enabler for new testmodes

uint32_t SHOWRATE = 0;
uint32_t SEEDPREFIXLEN = 30;
uint32_t GROUPSIZE = 3; // prefix table size is GROUPSIZE * path table size
uint32_t BLOCKSIZE = 3;
uint32_t TABLESIZE = 10;
uint32_t MAXBURSTCOUNT = 3; // path table size is MAXBURSTCOUNT * BLOCKSIZE
uint32_t NEXTHOP;
char sNEXTHOP[] = "192.168.1.1"; // = toHostAddress("192.168.1.1");  /// cant
                                 // initilase like this ;-(
uint32_t SEEDPREFIX;
char sSEEDPREFIX[] = "10.0.0.0"; // = toHostAddress("10.0.0.0");  /// cant
                                 // initilase like this ;-(
uint32_t CANARYSEED;
char sCANARYSEED[] = "192.168.255.0";
uint32_t CYCLECOUNT = 1; // 0 => continuous, use MAXBURSTCOUNT = 0 to suppress sending at all
uint32_t CYCLEDELAY = 5; // seconds
uint32_t HOLDTIME = 10000;

char LOGFILE[128] = "stats.csv";
char LOGPATH[128] = "localhost";
char LOGTEXT[1024] = "";
char *MODE;
char sMODE[128] = "BURST"; // only LISTENER and SENDER have any effect
char BURST[] = "BURST";
char DYNAMIC[] = "DYNAMIC";
char CONTINUOUS[] = "CONTINUOUS";

uint32_t IDLETHR = 1; // 1 seconds default burst idle threshold

void startpeer(struct peer *p, char *s) {

  parseargument(p, s);

  if (0 == p->remoteip) // servers have a zero 'remote' address
    die("server mode not supported in this version");

  int peersock;
  struct sockaddr_in peeraddr = {AF_INET, htons(179), (struct in_addr){p->remoteip}};
  struct sockaddr_in myaddr = {AF_INET, 0, (struct in_addr){p->localip}};

  0 < (peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) ||
      die("Failed to create socket");

  0 == bind(peersock, (const struct sockaddr *)&myaddr, SOCKADDRSZ) ||
      die("Failed to bind local address");

  0 == (connect(peersock, (const struct sockaddr *)&peeraddr, SOCKADDRSZ)) ||
      die("Failed to connect with peer");

  p->sock = peersock;

  fprintf(stderr, "connected for %s\n", s);
  pthread_create(&(p->thrd), NULL, establish, p);
  fprintf(stderr, "started for %s,%ld\n", s, p->thrd);
};

// NOTE - the target string must be actual static memory large enough...
void getsenv(char *name, char *tgt) {
  char *s;
  if (s = getenv(name)) {
    strcpy(tgt, s);
    fprintf(stderr, "read %s from environment: %s\n", name, s);
  };
};

void gethostaddress(char *name, uint32_t *tgt) {
  char *s;
  if ((s = getenv(name)) && (1 == sscanf(s, "%s", s))) {
    *tgt = toHostAddress(s);
    fprintf(stderr, "read %s from environment: %s\n", name, fromHostAddress(*tgt));
  };
};

void getuint32env(char *name, uint32_t *tgt) {
  char *s;
  uint32_t n;
  if ((s = getenv(name)) && (1 == sscanf(s, "%d", &n))) {
    *tgt = n;
    fprintf(stderr, "read %s from environment: %d\n", name, n);
  };
};

void getllienv(char *name, long long int *tgt) {
  char *s;
  long long int n;
  if ((s = getenv(name)) && (1 == sscanf(s, "%lld", &n))) {
    *tgt = n;
    fprintf(stderr, "read %s from environment: %lld\n", name, n);
  };
};

FILE *logfile = NULL;
void endlog(char *error) {
  if (NULL == logfile)
    fprintf(stderr, "endlog: logfile not opened\n");
  else {
    fprintf(logfile, "HDR , STOP, TIME, ERROR\nSTOP,%s,%s\n", shownow(), ((NULL == error) ? "" : error));
    fclose(logfile);
    logfile = NULL;
    if (0 != LOGPATH) {
      char *sp;
      time_t t = time(NULL);
      int tmp = asprintf(&sp, "curl -X PUT --data-binary @%s http://%s/%ld", LOGFILE, LOGPATH, t);
      int res = system(sp);
      if (0 == res)
        fprintf(stderr, "logging complete, results uploaded to http://%s/%ld\n", LOGPATH, t);
      else
        fprintf(stderr, "logging complete, failed to upload results to http://%s/%ld (%d)\n", LOGPATH, t, res);
      free(sp);
    };
  };
  if (NULL != error)
    fprintf(stderr, "abnormal termination, error msg: %s\n", error);
  /*
  if (NULL == error)
    exit(0);
  else {
    fprintf(stderr, "abnormal termination, error msg: %s\n", error);
    exit(1);
  };
*/
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
  if (NULL == logfile)
    fprintf(stderr, "rcvlog: logfile not opened\n");
  else {
    fprintf(logfile, "DATA, %d , %f , %f , %f , %f\n", seq,
            timespec_to_double(timespec_sub(*end, sndlog_start)),
            timespec_to_double(timespec_sub(*start, sndlog_end)),
            timespec_to_double(timespec_sub(sndlog_end, sndlog_start)),
            timespec_to_double(timespec_sub(*end, *start)));
  };
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

  setvbuf(stdout, NULL, _IOLBF, 0);
  setvbuf(stderr, NULL, _IOLBF, 0);
  pid = getpid();
  fprintf(stderr, "kakapo  Version %s (%s) \n", VERSION, BUILDDATE);
  if (1 > argc) {
    fprintf(stderr, "USAGE: kakapo {IP address[,IP address} [{IP address[,IP address}]\n");
    fprintf(stderr, "       many options are controlled via environment variables like SLEEP, etc...\n");
    exit(1);
  }

  0 == (sem_init(&semrxtx, 0, 0)) || die("semaphore create fail");
  NEXTHOP = toHostAddress(sNEXTHOP); /// must initliase here because cant do it in the declaration
  SEEDPREFIX = toHostAddress(sSEEDPREFIX);
  CANARYSEED = toHostAddress(sCANARYSEED);
  getuint32env("SLEEP", &SLEEP);
  getuint32env("TIMEOUT", &TIMEOUT);
  getuint32env("FASTCYCLELIMIT", &FASTCYCLELIMIT);
  getuint32env("IDLETHR", &IDLETHR);
  gethostaddress("SEEDPREFIX", &SEEDPREFIX);
  gethostaddress("CANARYSEED", &CANARYSEED);
  getuint32env("SEEDPREFIXLEN", &SEEDPREFIXLEN);
  getuint32env("GROUPSIZE", &GROUPSIZE);
  getuint32env("BLOCKSIZE", &BLOCKSIZE);
  getuint32env("TABLESIZE", &TABLESIZE);
  getuint32env("MAXBURSTCOUNT", &MAXBURSTCOUNT);
  gethostaddress("NEXTHOP", &NEXTHOP);
  getuint32env("CYCLECOUNT", &CYCLECOUNT);
  getuint32env("CYCLEDELAY", &CYCLEDELAY);
  getuint32env("SHOWRATE", &SHOWRATE);
  getuint32env("HOLDTIME", &HOLDTIME);
  getsenv("LOGFILE", LOGFILE);
  getsenv("LOGPATH", LOGPATH);
  getsenv("LOGTEXT", LOGTEXT);
  MODE = sMODE;
  getsenv("MODE", MODE);

  startstatsrunner();

  struct peer *peertable;
  int argn;
  struct peer *p;

  peertable = calloc(argc, sizeof(struct peer));
  for (argn = 1; argn <= argc - 1; argn++) {
    p = peertable + argn - 1;
    p->tidx = argn;
    if (argn == 1)
      p->role = ROLELISTENER;
    else
      p->role = ROLESENDER;
    startpeer(p, argv[argn]);
  };

  fprintf(stderr, "connection initiated for %d peers\n", argc - 1);

  for (argn = 0; argn < argc - 1; argn++)
    0 == pthread_join((peertable + argn)->thrd, NULL) || die("pthread join fail");

  fprintf(stderr, "connection complete for %d peers\n", argc - 1);

  if (0 == strcmp(MODE, "TEST")) {
    crf_test(peertable);
    fprintf(stderr, "crf_test complete\n");
    crf_canary_test(peertable);
    fprintf(stderr, "crf_canary_test complete\n");
  } else if (0 == strcmp(MODE, "MULTI")) {
    strict_canary_all(peertable);
    conditioning(peertable);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
  } else if (0 == strcmp(MODE, "BOTH")) {
    strict_canary_all(peertable);
    conditioning(peertable);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    multi_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
  } else {

    strict_canary_all(peertable);
    fprintf(stderr, "canary complete for %d peers\n", argc - 1);

    conditioning(peertable);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    single_peer_burst_test(peertable, MAXBURSTCOUNT);
    strict_canary_all(peertable);
    fprintf(stderr, "single_peer_burst_tests complete for %d peers\n", argc - 1);
  }

  notify_all(peertable);
  fprintf(stderr, "notification complete for %d peers\n", argc - 1);
  fprintf(stderr, "kakapo exit\n");
  exit(0);
}
