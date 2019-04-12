
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
#include "session.h"
#include "sockbuf.h"
#include "stats.h"
#include "util.h"

#define MAXPENDING 5 // Max connection requests

sem_t semrxtx;
struct timespec txts;

int pid;
int tidx = 0;
uint32_t MYAS = 65001;
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
    1; // 0 => continuous, use MAXBURSTCOUNT = 0 to suppress sending at all
uint32_t CYCLEDELAY = 30; // seconds
uint32_t HOLDTIME = 180;

char MYIP[16] = "0.0.0.0";
char LOGFILE[128] = "stats.csv";
char LOGTEXT[1024] = "";
uint32_t IDLETHR = 1; // 1 seconds default burst idle threshold

void startsession(int sock) {

  struct sessiondata *sd;
  sd = malloc(sizeof(struct sessiondata));
  *sd = (struct sessiondata){sock, tidx++, MYAS};
  if (1 == tidx)
    sd->role = ROLELISTENER;
  else if (2 == tidx)
    sd->role = ROLESENDER;
  pthread_t thrd;
  pthread_create(&thrd, NULL, session, sd);
};

void client(char *s) {
  int peersock;
  struct sockaddr_in peeraddr, myaddr;

  fprintf(stderr, "%d: Connecting to: %s\n", pid, s);

  // parse the parameter string for optional source address

  memset(&peeraddr, 0, SOCKADDRSZ);
  peeraddr.sin_family = AF_INET;
  peeraddr.sin_port = htons(179);

  memset(&myaddr, 0, SOCKADDRSZ);
  myaddr.sin_family = AF_INET;
  myaddr.sin_port = 0; // allow the OS to choose the outbound port (redundant as
                       // we did a memset)

  (0 < (peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) ||
   die("Failed to create socket"));

  if (0 ==
      inet_aton(s,
                &peeraddr.sin_addr)) { // failed to parse as a single address..
                                       // if there is no comma, it fails
    // if there is a comma then only if aton on both halves works is it ok...
    char *commaloc;
    if (0 != (commaloc = strchr(s, ','))) {
      *commaloc = 0;
      if (inet_aton(s, &peeraddr.sin_addr) &&
          inet_aton(commaloc + 1, &myaddr.sin_addr)) {
        (0 == bind(peersock, &myaddr, SOCKADDRSZ) ||
         die("Failed to bind local address"));
        fprintf(stderr, "%d: success using local address %s\n", pid,
                inet_ntoa(myaddr.sin_addr));
      } else
        die("Failed to parse address(es)");
    } else
      die("Failed to parse address(es)");
  };
  // fprintf(stderr, "%d: using peer address %s\n",pid,
  // inet_ntoa(peeraddr.sin_addr));

  (0 == (connect(peersock, (struct sockaddr *)&peeraddr, SOCKADDRSZ)) ||
   die("Failed to connect with peer"));

  // fprintf(stderr, "%d: Peer connected: %s\n",pid, s);
  startsession(peersock);
};

void server() {
  int serversock, peersock;
  int reuse = 1;
  socklen_t socklen;

  struct sockaddr_in acceptaddr, localaddr, peeraddr, hostaddr;

  memset(&hostaddr, 0, SOCKADDRSZ);
  hostaddr.sin_family = AF_INET;
  0 != inet_aton(MYIP, &hostaddr.sin_addr) ||
      die("Failed to read server bind address from environment");
  // hostaddr.sin_addr.s_addr = htonl(INADDR_ANY);   // local server addr -
  // wildcard - could be a specific interface
  hostaddr.sin_port = htons(179); // BGP server port

  0 == ((serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) ||
      die("Failed to create socket");

  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse,
                   sizeof(reuse)) < 0) ||
      die("Failed to set server socket option SO_REUSEADDR");

  0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char *)&reuse,
                   sizeof(reuse)) < 0) ||
      die("Failed to set server socket option SO_REUSEPORT");

  0 == (bind(serversock, (struct sockaddr *)&hostaddr, SOCKADDRSZ) < 0) ||
      die("Failed to bind the server socket");

  0 == (listen(serversock, MAXPENDING) < 0) ||
      die("Failed to listen on server socket");

  while (1) {

    memset(&acceptaddr, 0, SOCKADDRSZ);
    socklen = SOCKADDRSZ;
    0 < (peersock = accept(serversock, &acceptaddr, &socklen)) ||
        die("Failed to accept peer connection");
    (SOCKADDRSZ == socklen && AF_INET == acceptaddr.sin_family) ||
        die("bad sockaddr");
    startsession(peersock);
  }
};

// NOTE - the target string must be actual static memory large enough...
void getsenv(char *name, char *tgt) {
  char *s;
  // if ((s = getenv(name)) && (1 == sscanf(s, "%s", s))) {
  if (s = getenv(name)) {
    strcpy(tgt, s);
    fprintf(stderr, "%d: read %s from environment: %s\n", pid, name, s);
  };
};

void gethostaddress(char *name, uint32_t *tgt) {
  char *s;
  if ((s = getenv(name)) && (1 == sscanf(s, "%s", s))) {
    *tgt = toHostAddress(s);
    fprintf(stderr, "%d: read %s from environment: %s\n", pid, name,
            fromHostAddress(*tgt));
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
  fprintf(logfile, "end run at %s\n", shownow());
  exit(0);
};

void startlog(uint32_t tid, char *tids, struct timespec *start) {
  0 != (logfile = fopen(LOGFILE, "a")) || die("could not open log file");
  setvbuf(logfile, NULL, _IOLBF, 0);

  fprintf(stderr,
          "\n%s startlog at %s BLOCKSIZE %d, GROUPSIZE %d, MAXBURSTCOUNT %d, "
          "CYCLECOUNT %d, CYCLEDELAY %d\n",
          tids, showtime(start), BLOCKSIZE, GROUPSIZE, MAXBURSTCOUNT,
          CYCLECOUNT, CYCLEDELAY);

  fprintf(logfile,
          "new run, %d, desc \"%s\" at %s BLOCKSIZE %d, GROUPSIZE %d, "
          "MAXBURSTCOUNT %d, "
          "CYCLECOUNT %d, CYCLEDELAY %d\n",
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
  fprintf(logfile, "%d , %f , %f , %f , %f\n", seq,
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
    fprintf(
        stderr,
        "USAGE: kakapo {IP address[,IP address} [{IP address[,IP address}]\n");
    fprintf(stderr, "       many options are controlled via environment "
                    "variables like MYAS, MYIP, SLEEP, etc...\n");
    exit(1);
  }

  0 == (sem_init(&semrxtx, 0, 0)) || die("semaphore create fail");
  // sem_init(&semrxtx,0,0);
  NEXTHOP = toHostAddress(
      sNEXTHOP); /// must initliase here because cant do it in the declaration
  SEEDPREFIX = toHostAddress(sSEEDPREFIX); /// cant initilase like this ;-(
  getuint32env("MYAS", &MYAS);
  getuint32env("SLEEP", &SLEEP);
  getuint32env("TIMEOUT", &TIMEOUT);
  getsenv("MYIP", MYIP);
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
  getsenv("LOGTEXT", LOGTEXT);

  startstatsrunner();

  if (1 == argc) { // server mode.....
    server();
  } else { // client mode
    int argn;
    for (argn = 2; argn <= argc; argn++)
      client(argv[argn - 1]);
  }
  while (1)
    sleep(100);
}
