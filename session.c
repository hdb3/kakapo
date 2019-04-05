
/* kakapo-session - a BGP traffic source and sink */

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
#include "kakapo.h"

// setting sleep enables a millisecond based repeat loop
#ifndef SLEEP
#define SLEEP (1000000)
#endif
#define MAXPENDING 5    // Max connection requests
#define BUFFSIZE 0x10000

void *session(void *x){
struct sessiondata *sd = (struct sessiondata *) x;

struct logrecord {
    long long ts;
    int updates,nlri,withdrawn;
};

struct logrecord cumulative,current;

void initlogrecord () {
    cumulative.ts = getinttime();
    cumulative.updates=0;
    cumulative.nlri=0;
    cumulative.withdrawn=0;
    current.ts=cumulative.ts;
    current.updates=0;
    current.nlri=0;
    current.withdrawn=0;
};

void updatelogrecord (int nlri, int withdrawn) {
    current.updates++;
    current.nlri += nlri;
    current.withdrawn += withdrawn;
};

struct lograterecord {
    struct logrecord cumulative,current;
};

char _s_displaylogrecord [1000];
char * displaylogrecord () {
   long long int now = getinttime();
   snprintf(_s_displaylogrecord,999,"elapsed time : %f (%f) update msg cnt %d (%d) NLRI cnt %d (%d) withdrawn cnt %d (%d)" ,
      (now - current.ts ) / 1000000.0 ,
      (now - cumulative.ts ) / 1000000.0 ,
      current.updates ,
      cumulative.updates ,
      current.nlri ,
      cumulative.nlri ,
      current.withdrawn ,
      cumulative.withdrawn);
   return _s_displaylogrecord;
};

char _s_displaylograterecord [1000];
char * displaylograterecord (struct lograterecord l) {
   snprintf(_s_displaylograterecord,999,"elapsed time : %f (%f) update msg rate %d (%d) NLRI rate %d (%d) withdrawn rate %d (%d)" ,
      l.current.ts / 1000000.0 ,
      l.cumulative.ts / 1000000.0 ,
      l.current.updates ,
      l.cumulative.updates ,
      l.current.nlri ,
      l.cumulative.nlri ,
      l.current.withdrawn ,
      l.cumulative.withdrawn);
   return _s_displaylograterecord;
};

struct lograterecord getlograterecord () {

// ends the current interval as well as reporting on it

    struct lograterecord this;

    long long int now = getinttime();
    long long int deltaCumulative = now-cumulative.ts;
    long long int deltaCurrent = now-current.ts;
    this.cumulative.ts = deltaCumulative;
    this.current.ts = deltaCurrent;

// update cumulative counters from current
    cumulative.updates += current.updates;
    cumulative.nlri += current.nlri;
    cumulative.withdrawn += current.withdrawn;

// calculate current rates
    this.current.updates = current.updates * 1e6 / deltaCurrent; //all integer arithmetic!
    this.current.nlri = current.nlri * 1e6 / deltaCurrent; //all integer arithmetic!
    this.current.withdrawn = current.withdrawn * 1e6 / deltaCurrent; //all integer arithmetic!

// calculate cumulative rates
    this.cumulative.updates = cumulative.updates * 1e6 / deltaCumulative; //all integer arithmetic!
    this.cumulative.nlri = cumulative.nlri * 1e6 / deltaCumulative; //all integer arithmetic!
    this.cumulative.withdrawn = cumulative.withdrawn * 1e6 / deltaCumulative; //all integer arithmetic!

// reset the current counters
    current.ts = now;
    current.updates=0;
    current.nlri=0;
    current.withdrawn=0;

    return this;
};

unsigned char keepalive [19]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 19, 4 };
unsigned char marker [16]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
const char *hexmarker = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";

char *bgpopen(int as, int holdtime, int routerid, char *hexoptions) {
    char * hexmessage = concat (hex8(4), hex16(as), hex16(holdtime), hex32(routerid), hex8(strlen(hexoptions)/2), hexoptions, NULL);
    int messagelength = strlen(hexmessage) / 2 + 19;
    return concat (hexmarker,hex16(messagelength),hex8(1),hexmessage,NULL);
};

int isMarker (const unsigned char *buf) {
   return ( 0 == memcmp(buf,marker,16));
}

int i,msgtype;
struct sockbuf sb;
int msgcount = 0;
int reported_update_count = 0;
int update_count = 0;
int update_nlri_count = 0;
int update_withdrawn_count = 0;
struct timeval t_active, t_idle;
int active = 0;
int sock = sd->sock;
char *tid;
int tmp=asprintf(&tid,"%d-%d: ",pid,sd->tidx);

void setactive () {
    active = 1;
    gettimeofday(&t_active, NULL);
};

void setidle () {
    active = 0;
    gettimeofday(&t_idle, NULL);
};

char * showtype (unsigned char msgtype) {
   switch(msgtype) {
      case 1 : return "OPEN";
          break;
      case 2 : return "UPDATE";
          break;
      case 3 : return "NOTIFICATION";
          break;
      case 4 : return "KEEPALIVE";
          break;
      default : return "UNKNOWN";
    }
}

void doopen(char *msg, int length) {
   unsigned char version = * (unsigned char*) msg;
   if (version != 4) {
      fprintf(stderr, "%s: unexpected version in BGP Open %d\n",tid,version);
   }
   uint16_t as       = ntohs ( * (uint16_t*) (msg+1));
   uint16_t holdtime = ntohs ( * (uint16_t*) (msg+3));
   struct in_addr routerid = (struct in_addr) {* (uint32_t*) (msg+5)};
   unsigned char opl = * (unsigned char*) (msg+9);
   unsigned char *hex = toHex (msg+10,opl) ;
   fprintf(stderr, "%s: BGP Open: as =  %d, routerid = %s , holdtime = %d, opt params = %s\n",tid,as,inet_ntoa(routerid),holdtime,hex);
   free(hex);
};

void printPrefix(char *pfx, int length) {
    uint32_t addr = ntohl(* ((uint32_t*) pfx));
    uint32_t mask = (0xffffffff >> (32-length)) << (32-length);
    uint32_t maskedaddr = htonl( addr & mask);
    struct in_addr inaddr = (struct in_addr) {maskedaddr};
    fprintf(stderr,"%s/%d\n",inet_ntoa(inaddr),length);
};

//simpleParseNLRI
int spnlri (char *nlri, int length) {
    int count = 0;
    int offset = 0;
    for (;offset<length;) {
        count++;
        if (nlri[offset] == 0)
            offset += 1;
        else if (nlri[offset] < 9)
            offset += 2;
        else if (nlri[offset] < 17)
            offset += 3;
        else if (nlri[offset] < 25)
            offset += 4;
        else if (nlri[offset] < 33)
            offset += 5;
        else {
            unsigned char *hex = toHex (nlri,length) ;
            fprintf(stderr, "**** %d %d %d %d %s \n",nlri[offset],offset,count,length,hex);
            assert(0);
        }
        if ((1 == VERBOSE) && (offset<length))
            printPrefix(nlri+offset+1,nlri[offset]);
    }

    // debug only
    if (offset != length) {
        unsigned char *hex = toHex (nlri,length) ;
        fprintf(stderr, "**** %d %d %d %d %s \n",nlri[offset],offset,count,length,hex);
    }
    assert (offset==length);
    return count;
}

void doupdate(char *msg, int length) {
   uint16_t wrl = ntohs ( * (uint16_t*) msg);
   assert (wrl < length-1);
   char *withdrawn = msg+2;
   uint16_t tpal = ntohs ( * (uint16_t*) (msg+wrl+2));
   char *pa = msg+wrl+4;
   assert (wrl + tpal < length-3);
   char *nlri = msg+wrl+tpal+4;
   uint16_t nlril = length - wrl - tpal - 4;
   int wc,uc;
   if ( wrl > 0 )
        wc = spnlri(withdrawn,wrl);
   else
        wc = 0;
   if ( nlril > 0 )
        uc = spnlri(nlri,nlril);
   else
        uc = 0;
   if (1 == VERBOSE)
       fprintf(stderr, "%s: BGP Update: withdrawn count =  %d, path attributes length = %d , NLRI count = %d\n",tid,wc,tpal,uc);
   update_count ++;
   update_nlri_count += uc;
   update_withdrawn_count += wc;
   updatelogrecord (uc, wc);
};

void donotification(char *msg, int length) {
   unsigned char ec  = * (unsigned char*) (msg+0);
   unsigned char esc = * (unsigned char*) (msg+1);
   fprintf(stderr, "%s: BGP Notification: error code =  %d, error subcode = %d\n",tid,ec,esc);
};

int getBGPMessage (struct sockbuf *sb) {
   char *header;
   char *payload;
   int received;
   uint16_t pl;
   unsigned char msgtype;

   header = bufferedRead(sb,19);
   if (header == (char *) -1 ) {
      fprintf(stderr, "%s: end of stream\n",tid);
      return -1;
   } else if (0 == header ) {
      // zero is simply a timeout: -1 is eof
      if (active) {
          setidle();
      };
      return 0;
   } else if (!isMarker(header)) {
      die("Failed to find BGP marker in msg header from peer");
            return -1;
   } else {
      if (0==active) {
          setactive();
      };
      pl = ( ntohs ( * (uint16_t*) (header+16))) - 19;
      msgtype = * (unsigned char *) (header+18);
      if (0 < pl) {
         payload=bufferedRead(sb,pl);
         if (0 == payload) {
            fprintf(stderr, "%s: unexpected end of stream after header received\n",tid);
            return 0;
         }
     } else
         payload = 0;
   }
   msgcount++;
   if (1 == VERBOSE) {
      unsigned char *hex = toHex (payload,pl) ;
      fprintf(stderr, "%s: BGP msg type %s length %d received [%s]\n",tid, showtype(msgtype), pl , hex);
      free(hex);
   }

   switch (msgtype) {
      case 1:doopen(payload,pl);
             break;
      case 2:doupdate(payload,pl);
             break;
      case 3:donotification(payload,pl);
             break;
      case 4: // keepalive, no analysis required
             break;
   };
   return msgtype;
}

void report (int expected, int got) {

   if (1 == VERBOSE) {
      if (expected == got) {
         fprintf(stderr, "%s: session: OK, got %s\n",tid,showtype(expected));
      } else {
         fprintf(stderr, "%s: session: expected %s, got %s (%d)\n",tid,showtype(expected),showtype(got),got);
      }
   } else {
      if (expected != got)
         fprintf(stderr, "%s: session: expected %s, got %s (%d)\n",tid,showtype(expected),showtype(got),got);
   }
}

  void showstats ()  {
      struct timeval td0,td1,delay;
      delay.tv_sec = TIMEOUT;
      delay.tv_usec = 0;

      timeval_subtract(&td0,&t_idle,&t_active);
      timeval_subtract(&td1,&td0,&delay);
      fprintf(stderr, "%s: stats: msg cnt = %d, updates = %d, NLRIs = %d, withdrawn = %d, burst duration = %s\n",tid,msgcount,update_count,update_nlri_count,update_withdrawn_count,timeval_to_str(&td1));
      fprintf(stderr, "%s: counters: %s\n",tid,displaylogrecord ());
      fprintf(stderr, "%s: rate: %s\n",tid,displaylograterecord (getlograterecord ()));
      fprintf(stderr, "\e[4A\r\e[K\n");
  };


void *sendupdates (void *fd) {
  struct timeval t0, t1 , td;
  while (1) {
     lseek(*(int *)fd,0,0);
     gettimeofday(&t0, NULL);
     (0 < sendfile(sock, *(int *)fd, 0, 0x7ffff000)) || die("Failed to send updates to peer");
     gettimeofday(&t1, NULL);
     timeval_subtract(&td,&t1,&t0);
     fprintf(stderr, "%s: session: sendfile complete in %s\n",tid,timeval_to_str(&td));
     usleep(1000 * SLEEP);
   };
};

long int threadmain() {

  int fd1,fd2;
  if ((fd1 = open(sd->fn1,O_RDONLY)) < 0) {
    die("Failed to open BGP Open message file");
  }

  if ((fd2 = open(sd->fn2,O_RDONLY)) < 0) {
    die("Failed to open BGP Update message file");
  }


  msgcount = 0;
  reported_update_count = 0;
  update_count = 0;
  update_nlri_count = 0;
  update_withdrawn_count = 0;

  setsockopt( sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
  lseek(fd1,0,0);
  lseek(fd2,0,0);
  bufferInit(&sb,sock,BUFFSIZE,TIMEOUT);

  // (0 < sendfile(sock, fd1, 0, 0x7ffff000)) || die("Failed to send fd1 to peer");

  char * m = bgpopen(65001,180,htonl(inet_addr("192.168.122.123")),"020641040000fde8");
  int ml = fromHex(m);
  (0 < send(sock, m, ml, 0)) || die("Failed to send synthetic open to peer");

  do
      msgtype=getBGPMessage (&sb); // this is expected to be an Open
  while (msgtype==0);

  report(1,msgtype);
  if (1 != msgtype)
    goto exit;

  (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");

  do
      msgtype=getBGPMessage (&sb); // this is expected to be a Keepalive
  while (msgtype==0);

  report(4,msgtype);
  if (4 != msgtype)
    goto exit;

  pthread_t thrd;
  pthread_create(&thrd, NULL, sendupdates, &fd2);

  setidle();
  initlogrecord();  // implies that the rate display is based at first recv request call rather than return......
                    // for more precision consider moving to either getBGPMessage
                    // would be too late otherwise anywhere in here, as getBGPMessage will call updatelog

  while (1) {
        msgtype = getBGPMessage (&sb); // keepalive or updates from now on
        switch (msgtype) {
        case 2: // Update
            break;
        case 4: // Keepalive
            (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");
            break;
        case 0: // this is an idle recv timeout event
            if (reported_update_count != update_count) {
                reported_update_count = update_count;
                showstats();
            }
            break;
        case 3: // Notification
            fprintf(stderr, "%s: session: got Notification\n",tid);
            goto exit;
        default:
            if (msgtype<0){ // all non message events except recv timeout
                fprintf(stderr, "%s: session: end of stream\n",tid);
            } else { // unexpected BGP message - unless BGP++ it must be an Open....
                report(2,msgtype);
            }
            goto exit;
        }
  };
exit:
  pthread_cancel(thrd);
  close(sock);
  close(fd1);
  close(fd2);
  fprintf(stderr, "\n\n\n\n\n%s: session exit\n",tid);
  showstats();
  free(sd);
}

return (int*)threadmain();
}
