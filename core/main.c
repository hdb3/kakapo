
/* kakapo - a BGP traffic source and sink */

#include <arpa/inet.h>
#include <assert.h>
#include <bits/local_lim.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>
#include <uuid/uuid.h>

#include "kakapo.h"
#include "libutil.h"

#define MAXPENDING 5 // Max connection requests

uuid_t uuid;
char UUID[37];
char HOSTNAME[HOST_NAME_MAX];
sem_t semrxtx;
struct timespec txts;
struct peer *peertable = NULL;
struct peer *listener = NULL;
struct peer *senders = NULL;
int peer_count = 0;
int sender_count = 0;

int pid;
int tflag = 0; // the global termination flag - when != 0, exit gracefully
double conditioning_duration = 0.0;

uint32_t MAXBLOCKINGFACTOR = 1000;
uint32_t TIMEOUT = 10;
uint32_t REPEAT = 5;

uint32_t TCPPORT = 179;
uint32_t PEERMAXRETRIES = -1; // -1 ~ retry forever
uint32_t SHOWRATE = 0;
uint32_t SEEDPREFIXLEN = 30;
uint32_t GROUPSIZE = 3;
uint32_t WINDOW = 1000;
uint32_t TABLESIZE = 10;
uint32_t MAXBURSTCOUNT = 3;
uint32_t RATEBLOCKSIZE = 1000000;
uint32_t RATECOUNT = UINT32_MAX;
uint32_t RATETIMELIMIT = UINT32_MAX;
// if neither RATETIMELIMIT nor RATECOUNT set then a default is used for RATETIMELIMIT
#define RATETIMELIMIT_DEFAULT 10

uint32_t SEEDPREFIX;
char sSEEDPREFIX[] = "10.0.0.0";
uint32_t CANARYSEED;
char sCANARYSEED[] = "192.168.255.0";
uint32_t REPEATDELAY = 5; // seconds
uint32_t HOLDTIME = 10000;

char LOGFILE[128] = "stats.csv";
char LOGPATH[128] = "localhost";
char LOGTEXT[1024] = "";
char *MODE;
char SENDFILENAME[128] = "updates.raw";
char sMODE[128] = "BURST";
// char BURST[] = "BURST";
char DYNAMIC[] = "DYNAMIC";
char CONTINUOUS[] = "CONTINUOUS";

uint32_t IDLETHR = 1; // 1 seconds default burst idle threshold

static char *_s;
char *show_peer(struct peer *p) {
  int tmp = asprintf(&_s, "peer %d %d:%s", p->tidx, p->as, inet_ntoa((struct in_addr){p->localip}));
  return _s;
};

void startpeer(struct peer *p, char *s) {

  parseargument(p, s);

  if (0 == p->remoteip) // servers have a zero 'remote' address
    die("server mode not supported in this version");

  int peersock;
  int retries = 0;
  uint16_t port;
  if (0 == p->port)
    port = TCPPORT;
  else
    port = p->port;
  fprintf(stderr, "connecting to %s:%hd (%hd) (%hd)\n", inet_ntoa((struct in_addr){p->remoteip}), htons(port), port, p->port);
  struct sockaddr_in peeraddr = {AF_INET, htons(port), (struct in_addr){p->remoteip}};
  struct sockaddr_in myaddr = {AF_INET, 0, (struct in_addr){p->localip}};

  0 < (peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) ||
      die("Failed to create socket");

  0 == bind(peersock, (const struct sockaddr *)&myaddr, SOCKADDRSZ) ||
      die("Failed to bind local address");

  while (1) {
    if (0 == (connect(peersock, (const struct sockaddr *)&peeraddr, SOCKADDRSZ)))
      break;
    else if (retries++ == PEERMAXRETRIES)
      die("Failed to connect with peer");
    else {
      fprintf(stderr, "retrying connection\n");
      sleep(1);
    };
  };

  p->sock = peersock;

  p->thrd = _pthread_create((void *)establish, p);
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
};

void startlog(uint32_t tid, char *tids, struct timespec *start) {
  0 != (logfile = fopen(LOGFILE, "w")) || die("could not open log file");
  setvbuf(logfile, NULL, _IOLBF, 0);

  fprintf(stderr, "\n%s startlog at %s, GROUPSIZE %d, MAXBURSTCOUNT %d\n",
          tids, showtime(start), GROUPSIZE, MAXBURSTCOUNT);

  fprintf(logfile,
          "HDR , PID , DESC , START, GROUPSIZE, MAXBURSTCOUNT\n"
          "START, %d, \"%s\" , \"%s\" ,  %d, %d\n"
          "HDR , SEQ , RTT , LATENCY , TXDURATION, RXDURATION\n",
          pid, LOGTEXT, showtime(start), GROUPSIZE, MAXBURSTCOUNT);
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
  assert(seq == sndlog_seq);
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

FILE *loglocal = NULL;
FILE *logjson = NULL;
int multi_rate = 0;
int single_rate = 0;

void json_log(FILE *f, char *test_name, struct timespec *now, int sender_count, double conditioning_duration, double mean, double max, double min, double sd, int single_rate, int multi_rate) {
  fprintf(f, "{\n");

  fprintf(f, "\"unixtime\":%ld,", now->tv_sec);

  fprintf(f, "\"LOGTEXT\":\"%s\",", LOGTEXT);

  fprintf(f, "\"test_name\":\"%s\",", test_name);

  fprintf(f, "\"time\":\"%s\",", showtime(now));

  fprintf(f, "\"sender_count\":%d,", sender_count);

  fprintf(f, "\"conditioning_duration\":%f,", conditioning_duration);

  fprintf(f, "\"mean\":%f,", mean);

  fprintf(f, "\"max\":%f,", max);

  fprintf(f, "\"min\":%f,", min);

  fprintf(f, "\"sd\":%f,", sd);

  fprintf(f, "\"TABLESIZE\":%d,", TABLESIZE);

  fprintf(f, "\"GROUPSIZE\":%d,", GROUPSIZE);

  fprintf(f, "\"MAXBURSTCOUNT\":%d,", MAXBURSTCOUNT);

  fprintf(f, "\"REPEAT\":%d,", REPEAT);

  fprintf(f, "\"WINDOW\":%d,", WINDOW);

  fprintf(f, "\"RATECOUNT\":%d,", RATECOUNT);

  fprintf(f, "\"single_rate\":%d,", single_rate);

  fprintf(f, "\"multi_rate\":%d,", multi_rate);

  fprintf(f, "\"BRANCH\":\"%s\",", BRANCH);

  fprintf(f, "\"VERSION\":\"%s\",", VERSION);

  fprintf(f, "\"HOSTNAME\":\"%s\",", HOSTNAME);

  fprintf(f, "\"UUID\":\"%s\"", UUID);

  fprintf(f, "},\n");
};

void summarise(char *test_name, double *r) {
  struct timespec now;
  gettime(&now);

  if (REPEAT > 1) {
    double count = REPEAT - 1;
    double max = 0;
    double min = 0;
    double sum = 0;
    double sqsum = 0;

    for (int i = 1; i < REPEAT; i++) {
      sum += r[i];
      sqsum += r[i] * r[i];
      max = r[i] > max ? r[i] : max;
      min = 0 == min ? r[i] : (r[i] < min ? r[i] : min);
    };
    double mean = sum / count;
    double sd = sqrt((count * sqsum) - (sum * sum)) / count;
    double rsd = sd / mean;

    fprintf(stderr, "%s mean=%f max=%f min=%f\n", test_name, mean, max, min);
    fprintf(loglocal, "\"%s\" %s \"%s\" %d %f %f %f %f %f %d %d %d %d %d %d %d %d\n", LOGTEXT, test_name, showtime(&now), sender_count, conditioning_duration, mean, max, min, sd, TABLESIZE, GROUPSIZE, MAXBURSTCOUNT, REPEAT, WINDOW, RATECOUNT, single_rate, multi_rate);
    json_log(logjson, test_name, &now, sender_count, conditioning_duration, mean, max, min, sd, single_rate, multi_rate);
  } else
    json_log(logjson, test_name, &now, sender_count, conditioning_duration, *r, *r, *r, 0, single_rate, multi_rate);
};

// rate_summarise() uses global variables single_rate and multi_rate for legacy reasons -  multipart tests, e.g. 'PAM'.
void rate_summarise(char *test_name) {
  struct timespec now;
  gettime(&now);
  json_log(logjson, test_name, &now, sender_count, conditioning_duration, 0, 0, 0, 0, single_rate, multi_rate);
}

int main(int argc, char *argv[]) {

  0 != (logjson = fopen("kakapo.json", "a")) || die("could not open logjson file");
  int loglocalcheck = access("kakapo.log", F_OK);
  0 != (loglocal = fopen("kakapo.log", "a")) || die("could not open loglocal file");
  if (-1 == loglocalcheck) // write a header line in an empty file
    fprintf(loglocal, "LOGTEXT TEST TIME SENDERS CONDITIONING MEAN MAX MIN STDDEV TABLESIZE GROUPSIZE MAXBURSTCOUNT REPEAT WINDOW RATECOUNT SINGLERATE MULTIRATE\n");
  sigset_t set;
  uuid_t uuid;
  uuid_generate(uuid);
  uuid_unparse(uuid, UUID);
  gethostname(HOSTNAME, HOST_NAME_MAX);

  setvbuf(stdout, NULL, _IOLBF, 0);
  setvbuf(stderr, NULL, _IOLBF, 0);
  sigemptyset(&set);
  sigaddset(&set, SIGPIPE);
  pthread_sigmask(SIG_BLOCK, &set, NULL);
  pid = getpid();
  fprintf(stderr, "kakapo  version: %s  built: %s  branch: %s\n", VERSION, BUILDDATE, BRANCH);

  if ((argc > 1) && ((0 == strcmp(argv[1], "-V")) || (0 == strcmp(argv[1], "--version")))) {
    exit(0);
  }

  if (3 > argc) {
    fprintf(stderr, "USAGE: kakapo <IP address>[,<IP address>] <IP address>[,<IP address>] [<IP address>[,<IP address>]]\n");
    fprintf(stderr, "       Note the minimum number of peers is two, of which the first is a listener and all others are senders.\n");
    fprintf(stderr, "       Many options are controlled via environment variables like REPEAT, etc...\n");
    exit(1);
  }

  0 == (sem_init(&semrxtx, 0, 0)) || die("semaphore create fail");
  SEEDPREFIX = toHostAddress(sSEEDPREFIX);
  CANARYSEED = toHostAddress(sCANARYSEED);
  getuint32env("TIMEOUT", &TIMEOUT);
  getuint32env("RATEBLOCKSIZE", &RATEBLOCKSIZE);
  getuint32env("MAXBLOCKINGFACTOR", &MAXBLOCKINGFACTOR);
  getuint32env("REPEAT", &REPEAT);
  getuint32env("IDLETHR", &IDLETHR);
  gethostaddress("SEEDPREFIX", &SEEDPREFIX);
  gethostaddress("CANARYSEED", &CANARYSEED);
  getuint32env("SEEDPREFIXLEN", &SEEDPREFIXLEN);
  getuint32env("GROUPSIZE", &GROUPSIZE);
  getuint32env("WINDOW", &WINDOW);
  getuint32env("TABLESIZE", &TABLESIZE);
  getuint32env("MAXBURSTCOUNT", &MAXBURSTCOUNT);
  getuint32env("RATECOUNT", &RATECOUNT);
  getuint32env("RATETIMELIMIT", &RATETIMELIMIT);
  if ((RATETIMELIMIT == UINT32_MAX) && (RATECOUNT == UINT32_MAX)) {
    fprintf(stderr, "Neither RATETIMELIMIT nor RATECOUNT set, using RATETIMELIMIT = %d.\n", RATETIMELIMIT_DEFAULT);
    RATETIMELIMIT = RATETIMELIMIT_DEFAULT;
  }
  getuint32env("PEERMAXRETRIES", &PEERMAXRETRIES);
  getuint32env("REPEATDELAY", &REPEATDELAY);
  getuint32env("TCPPORT", &TCPPORT);
  getuint32env("SHOWRATE", &SHOWRATE);
  getuint32env("HOLDTIME", &HOLDTIME);
  getsenv("LOGFILE", LOGFILE);
  getsenv("LOGPATH", LOGPATH);
  getsenv("LOGTEXT", LOGTEXT);
  MODE = sMODE;
  getsenv("MODE", MODE);
  getsenv("SENDFILENAME", SENDFILENAME);

  int i;
  struct peer *p;
  double *results = malloc(REPEAT * sizeof(double));
  double *results2 = malloc(REPEAT * sizeof(double));

  peer_count = argc - 1;
  sender_count = peer_count - 1;
  assert(sender_count > 0);
  peertable = calloc(argc, sizeof(struct peer));
  listener = peertable;
  senders = peertable + 1;

  for (i = 0; i < peer_count; i++) {
    p = peertable + i;
    if (i == 0)
      p->role = ROLELISTENER;
    else
      p->role = ROLESENDER;
    p->tidx = i;
    startpeer(p, argv[i + 1]);
  };

  fprintf(stderr, "connection initiated for %d peers\n", peer_count);

  for (i = 0; i < peer_count; i++)
    0 == pthread_join((peertable + i)->thrd, NULL) || die("pthread join fail");

  fprintf(stderr, "connection complete for %d peers\n", peer_count);

  if (0 == strcmp(MODE, "SINGLEONLY")) {
    for (i = 0; i < REPEAT; i++) {
      canary_all();
      fprintf(stderr, "cycle %d\n", i);
      results[i] = single_peer_burst_test(MAXBURSTCOUNT);
      canary_all();
    };
    conditioning_duration = results[0];
    summarise("single_only_peer_burst_test", results);
  } else if (0 == strcmp(MODE, "SINGLE")) {
    conditioning_duration = conditioning();
    for (i = 0; i < REPEAT; i++) {
      canary_all();
      sleep(REPEATDELAY);
      fprintf(stderr, "cycle %d\n", i);
      results[i] = single_peer_burst_test(MAXBURSTCOUNT);
      keepalive_all();
    };
    sleep(REPEATDELAY);
    canary_all();
    summarise("single_peer_burst_test", results);
  } else if (0 == strcmp(MODE, "FILE")) {
    conditioning_duration = file_test(SENDFILENAME);
    for (i = 1; i < REPEAT; i++) {
      keepalive_all();
      sleep(REPEATDELAY);
      fprintf(stderr, "cycle %d\n", i);
      results[i] = file_test(SENDFILENAME);
    };
    summarise("file_test", results);
  } else if (0 == strcmp(MODE, "PAM")) {
    conditioning_duration = conditioning();
    for (i = 0; i < REPEAT; i++) {
      canary_all();
      sleep(REPEATDELAY);
      fprintf(stderr, "cycle %d\n", i);
      results[i] = single_peer_burst_test(MAXBURSTCOUNT);
      keepalive_all();
    };
    single_rate = single_peer_rate_test(RATECOUNT, WINDOW);
    multi_rate = multi_peer_rate_test(RATECOUNT, WINDOW);
    summarise("PAM", results);
  } else if (0 == strcmp(MODE, "MULTI")) {
    conditioning_duration = conditioning();
    for (i = 0; i < REPEAT; i++) {
      canary_all();
      sleep(REPEATDELAY);
      fprintf(stderr, "cycle %d\n", i);
      results[i] = multi_peer_burst_test(MAXBURSTCOUNT);
      keepalive_all();
    };
    summarise("multi_peer_burst_test", results);
  } else if (0 == strcmp(MODE, "BOTH")) {
    conditioning_duration = conditioning();
    for (i = 0; i < REPEAT; i++) {
      canary_all();
      sleep(REPEATDELAY);
      fprintf(stderr, "cycle %d\n", i);
      results[i] = single_peer_burst_test(MAXBURSTCOUNT);
      canary_all();
      sleep(REPEATDELAY);
      results2[i] = multi_peer_burst_test(MAXBURSTCOUNT);
      keepalive_all();
    };
    summarise("single_peer_burst_test", results);
    summarise("multi_peer_burst_test", results2);
  } else if (0 == strcmp(MODE, "RATE")) {
    fprintf(stderr, "rate test mode\n");
    fprintf(stderr, "MESSAGE COUNT %d  WINDOW %d\n", RATECOUNT, WINDOW);
    canary_all();
    conditioning_duration = conditioning();
    canary_all();
    sleep(1);
    multi_rate = multi_peer_rate_test(RATECOUNT, WINDOW);
    rate_summarise("multi_peer_rate_test");
  } else if (0 == strcmp(MODE, "SINGLERATE")) {
    fprintf(stderr, "single peer rate test mode\n");
    fprintf(stderr, "MESSAGE COUNT %d  WINDOW %d\n", RATECOUNT, WINDOW);
    canary_all();
    conditioning_duration = conditioning();
    canary_all();
    sleep(1);
    single_rate = single_peer_rate_test(RATECOUNT, WINDOW);
    rate_summarise("single_peer_rate_test");
  } else if (0 == strcmp(MODE, "FUNCTEST")) {
    fprintf(stderr, "single peer functional test mode\n");
    canary_all();
    conditioning_duration = conditioning();
    canary_all();
    sleep(1);
    single_peer_func_test(MAXBURSTCOUNT);
  } else {
    fprintf(stderr, "default test mode - single_peer_func_test\n");
    single_peer_func_test(MAXBURSTCOUNT);
  }

  notify_all();
  fprintf(stderr, "notification complete for %d peers\n", argc - 1);
  fprintf(stderr, "kakapo exit\n");

  exit(0);
}
