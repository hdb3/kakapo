
/* kakapo - a BGP traffic source and sink */

#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/sendfile.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/time.h>
#include <pthread.h>

#include "sockbuf.h"
#include "util.h"
#include "session.h"
#include "stats.h"
#include "kakapo.h"

#define MAXPENDING 5    // Max connection requests

int pid;
int tidx = 0;
uint32_t MYAS = 65001;
uint32_t SLEEP = 0; // default value -> don't repeat the send operation
uint32_t TIMEOUT = 10;

uint32_t SEEDPREFIXLEN = 30;
uint32_t GROUPSIZE = 3;
uint32_t BLOCKSIZE = 3;
uint32_t MAXBURSTCOUNT = 3;
uint32_t NEXTHOP;    char sNEXTHOP []    = "192.168.1.1"; // = toHostAddress("192.168.1.1");  /// cant initilase like this ;-(
uint32_t SEEDPREFIX; char sSEEDPREFIX [] = "10.0.0.0"; // = toHostAddress("10.0.0.0");  /// cant initilase like this ;-(

char MYIP [16] = "0.0.0.0";
long long int idlethreshold = (long long int) 1e10; // 10 seconds default burst idle threshold

void startsession(int sock ) {

  struct sessiondata *sd;
  sd = malloc (sizeof(struct sessiondata));
  *sd = (struct sessiondata) { sock , tidx++, MYAS };
  pthread_t thrd;
  pthread_create(&thrd, NULL, session, sd);

};

void client (char *s) {
  int peersock;
  struct sockaddr_in peeraddr,myaddr;

  fprintf(stderr, "%d: Connecting to: %s\n",pid, s);

  // parse the parameter string for optional source address

  memset(&peeraddr, 0, SOCKADDRSZ );
  peeraddr.sin_family = AF_INET;
  peeraddr.sin_port = htons(179);


  memset(&myaddr, 0, SOCKADDRSZ );
  myaddr.sin_family = AF_INET;
  myaddr.sin_port = 0; // allow the OS to choose the outbound port (redundant as we did a memset)

  (0 < (peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP) ) || die ("Failed to create socket"));

  if (0 == inet_aton(s,&peeraddr.sin_addr)) { // failed to parse as a single address..
  // if there is no comma, it fails
  // if there is a comma then only if aton on both halves works is it ok...
    char * commaloc;
    if ( 0 != (commaloc = strchr (s,','))) {
        *commaloc = 0;
        if (inet_aton(s,&peeraddr.sin_addr) && inet_aton(commaloc+1,&myaddr.sin_addr)) {
            (0 == bind(peersock,&myaddr,SOCKADDRSZ) || die ("Failed to bind local address"));
            fprintf(stderr, "%d: success using local address %s\n",pid, inet_ntoa(myaddr.sin_addr));
        } else
            die ("Failed to parse address(es)");
    } else
        die ("Failed to parse address(es)");
  };
  //fprintf(stderr, "%d: using peer address %s\n",pid, inet_ntoa(peeraddr.sin_addr));

  (0 == (connect(peersock, (struct sockaddr *) &peeraddr, SOCKADDRSZ) ) || die ("Failed to connect with peer"));

  //fprintf(stderr, "%d: Peer connected: %s\n",pid, s);
  startsession( peersock );
};


void server() {
  int serversock, peersock;
  int reuse = 1;
  socklen_t socklen;

  struct sockaddr_in acceptaddr,localaddr, peeraddr,hostaddr;

    memset(&hostaddr, 0, SOCKADDRSZ );
    hostaddr.sin_family = AF_INET;
    0 != inet_aton(MYIP,&hostaddr.sin_addr) || die("Failed to read server bind address from environment");
    //hostaddr.sin_addr.s_addr = htonl(INADDR_ANY);   // local server addr - wildcard - could be a specific interface
    hostaddr.sin_port = htons(179);       // BGP server port

    0 == ((serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) || die("Failed to create socket");

    0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char*)&reuse, sizeof(reuse)) < 0) || die("Failed to set server socket option SO_REUSEADDR");

    0 == (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char*)&reuse, sizeof(reuse)) < 0) || die("Failed to set server socket option SO_REUSEPORT");

    0 == (bind(serversock, (struct sockaddr *) &hostaddr, SOCKADDRSZ ) < 0) || die("Failed to bind the server socket");

    0 == (listen(serversock, MAXPENDING) < 0) || die("Failed to listen on server socket");

    while (1) {

      memset(&acceptaddr, 0, SOCKADDRSZ); socklen = SOCKADDRSZ; 0 <  ( peersock = accept(serversock, &acceptaddr, &socklen )) || die("Failed to accept peer connection");
      ( SOCKADDRSZ == socklen && AF_INET == acceptaddr.sin_family) || die("bad sockaddr");
      startsession( peersock );
    }
};

// NOTE - the target string must be actual static memory large enough...
void getsenv(char* name , char* tgt) {
  char* s;
  if ( (s = getenv(name)) && (1 == sscanf(s,"%s",s))) {
    strcpy(tgt,s);
    fprintf(stderr, "%d: read %s from environment: %s\n",pid,name,s);
  };
};

void gethostaddress(char* name , uint32_t* tgt) {
  char* s;
  if ( (s = getenv(name)) && (1 == sscanf(s,"%s",s))) {
    *tgt = toHostAddress(s);
    fprintf(stderr, "%d: read %s from environment: %s\n",pid,name,fromHostAddress(*tgt));
  };
};

void getuint32env(char* name , uint32_t* tgt) {
  char* s;
  uint32_t n;
  if ( (s = getenv(name)) && (1 == sscanf(s,"%d",&n))) {
    *tgt = n;
    fprintf(stderr, "%d: read %s from environment: %d\n",pid,name,n);
  };
};

void getllienv(char* name , long long int* tgt) {
  char* s;
  long long int n;
  if ( (s = getenv(name)) && (1 == sscanf(s,"%lld",&n))) {
    *tgt = n;
    fprintf(stderr, "%d: read %s from environment: %lld\n",pid,name,n);
  };
};

int main(int argc, char *argv[]) {

  pid = getpid();
  fprintf(stderr, "%d: kakapo\n",pid);
  if (1 > argc) {
      fprintf(stderr, "USAGE: kakapo {IP address[,IP address} [{IP address[,IP address}]\n");
      fprintf(stderr, "       many options are controlled via environment variables like MYAS, MYIP, SLEEP, etc...\n");
      exit(1);
  }

  NEXTHOP = toHostAddress(sNEXTHOP);  /// must initliase here because cant do it in the declaration
  SEEDPREFIX = toHostAddress(sSEEDPREFIX);  /// cant initilase like this ;-(
  getuint32env("MYAS",&MYAS);
  getuint32env("SLEEP",&SLEEP);
  getuint32env("TIMEOUT",&TIMEOUT);
  getsenv("MYIP",MYIP);
  getllienv("IDLETHR",&idlethreshold);
  gethostaddress("SEEDPREFIX" , &SEEDPREFIX);
  getuint32env("SEEDPREFIXLEN" , &SEEDPREFIXLEN);
  getuint32env("GROUPSIZE" , &GROUPSIZE);
  getuint32env("BLOCKSIZE" , &BLOCKSIZE);
  getuint32env("MAXBURSTCOUNT" , &MAXBURSTCOUNT);
  getuint32env("NEXTHOP" , &NEXTHOP);

  startstatsrunner ();

  if (1 == argc) { // server mode.....
    server();
  } else { // client mode
     int argn;
     for (argn=2 ; argn <= argc ; argn++)
         client(argv[argn-1]);
  }
  while (1)
    sleep(100);
}
