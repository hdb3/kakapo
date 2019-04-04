
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

#define MAXPENDING 5    // Max connection requests
#define BUFFSIZE 0x10000
#define SOCKADDRSZ (sizeof (struct sockaddr_in))

//void session(int sock, char * fn1 , char * fn2) {
//*void session(struct sessiondata *sd);
void *session(void *x){
struct sessiondata *sd = (struct sessiondata *) x;

unsigned char keepalive [19]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 19, 4 };
unsigned char marker [16]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
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
      fprintf(stderr, "%d: unexpected version in BGP Open %d\n",pid,version);
   }
   uint16_t as       = ntohs ( * (uint16_t*) (msg+1));
   uint16_t holdtime = ntohs ( * (uint16_t*) (msg+3));
   struct in_addr routerid = (struct in_addr) {* (uint32_t*) (msg+5)};
   unsigned char opl = * (unsigned char*) (msg+9);
   unsigned char *hex = toHex (msg+10,opl) ;
   fprintf(stderr, "%d: BGP Open: as =  %d, routerid = %s , holdtime = %d, opt params = %s\n",pid,as,inet_ntoa(routerid),holdtime,hex);
   free(hex);
};

void printPrefix(char *pfx, int length) {
    uint32_t addr = ntohl(* ((uint32_t*) pfx));
    uint32_t mask = (0xffffffff >> (32-length)) << (32-length);
    uint32_t maskedaddr = htonl( addr & mask);
    struct in_addr inaddr = (struct in_addr) {maskedaddr};
    // fprintf(stderr,"%s/%d\n",inet_ntoa(inaddr),length);
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
        if (offset<length)
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
   //fprintf(stderr, "%d: BGP Update: withdrawn length =  %d, path attributes length = %d , NLRI length = %d\n",pid,wrl,tpal,nlril);
   int wc,uc;
   if ( wrl > 0 )
        wc = spnlri(withdrawn,wrl);
   else
        wc = 0;
   if ( nlril > 0 )
        uc = spnlri(nlri,nlril);
   else
        uc = 0;
   if VERBOSE
       fprintf(stderr, "%d: BGP Update: withdrawn count =  %d, path attributes length = %d , NLRI count = %d\n",pid,wc,tpal,uc);
   update_count ++;
   update_nlri_count += uc;
   update_withdrawn_count += wc;
};

void donotification(char *msg, int length) {
   unsigned char ec  = * (unsigned char*) (msg+0);
   unsigned char esc = * (unsigned char*) (msg+1);
   fprintf(stderr, "%d: BGP Notification: error code =  %d, error subcode = %d\n",pid,ec,esc);
};

int getBGPMessage (struct sockbuf *sb) {
   char *header;
   char *payload;
   int received;
   uint16_t pl;
   unsigned char msgtype;

   header = bufferedRead(sb,19);
   if (header == (char *) -1 ) {
      fprintf(stderr, "%d: end of stream\n",pid);
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
            fprintf(stderr, "%d: unexpected end of stream after header received\n",pid);
            return 0;
         }
     } else
         payload = 0;
   }
   msgcount++;
   if VERBOSE {
      unsigned char *hex = toHex (payload,pl) ;
      fprintf(stderr, "%d: BGP msg type %s length %d received [%s]\n",pid, showtype(msgtype), pl , hex);
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

   if VERBOSE {
      if (expected == got) {
         fprintf(stderr, "%d: session: OK, got %s\n",pid,showtype(expected));
      } else {
         fprintf(stderr, "%d: session: expected %s, got %s (%d)\n",pid,showtype(expected),showtype(got),got);
      }
   } else {
      if (expected != got) 
         fprintf(stderr, "%d: session: expected %s, got %s (%d)\n",pid,showtype(expected),showtype(got),got);
   }
}

  void showstats ()  {
      struct timeval td0,td1,onesec;
      onesec.tv_sec = 1;
      onesec.tv_usec = 0;

      timeval_subtract(&td0,&t_idle,&t_active);
      timeval_subtract(&td1,&td0,&onesec);
      fprintf(stderr, "%d: stats: msg cnt = %d, updates = %d, NLRIs = %d, withdrawn = %d, burst duration = %s\n",pid,msgcount,update_count,update_nlri_count,update_withdrawn_count,timeval_to_str(&td1));
  };

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
  bufferInit(&sb,sock,BUFFSIZE);

  (0 < sendfile(sock, fd1, 0, 0x7ffff000)) || die("Failed to send fd1 to peer");

  do
      msgtype=getBGPMessage (&sb); // this is expected to be an Open
  while (msgtype==0);

  report(1,msgtype);

  (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");

  do
      msgtype=getBGPMessage (&sb); // this is expected to be a Keepalive
  while (msgtype==0);

  report(4,msgtype);

void *sendupdates (void *fd) {
  struct timeval t0, t1 , td;
  gettimeofday(&t0, NULL);
  (0 < sendfile(sock, *(int *)fd, 0, 0x7ffff000)) || die("Failed to send updates to peer");
  gettimeofday(&t1, NULL);
  timeval_subtract(&td,&t1,&t0);
  fprintf(stderr, "%d: session: sendfile complete in %s\n",pid,timeval_to_str(&td));
};

  //*sendupdates((void *)&fd2);
  pthread_t thrd;
  pthread_create(&thrd, NULL, sendupdates, &fd2);

  setidle();

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
            fprintf(stderr, "%d: session: got Notification\n",pid);
            goto exit;
        default:
            if (msgtype<0){ // all non message events except recv timeout
                fprintf(stderr, "%d: session: end of stream\n",pid);
            } else { // unexpected BGP message - unless BGP++ it must be an Open....
                report(2,msgtype);
            }
            goto exit;
        }
  };
exit:
  close(sock);
  // bufferClose();
  fprintf(stderr, "%d: session exit\n",pid);
  showstats();
}