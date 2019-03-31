
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

#include "sockbuf.h"
#include "util.h"

#define MAXPENDING 5    // Max connection requests
#define BUFFSIZE 0x10000
#define SOCKADDRSZ (sizeof (struct sockaddr_in))
#define VERBOSE (0)

int die(char *mess) { perror(mess); exit(1); }
unsigned char keepalive [19]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 19, 4 };
unsigned char marker [16]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
int isMarker (const unsigned char *buf) {
   return ( 0 == memcmp(buf,marker,16));
}

int msgcount = 0;
int update_count = 0;
int update_nlri_count = 0;
int update_withdrawn_count = 0;
int pid;

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
    assert (offset==length);
    return count;
}

void doupdate(char *msg, int length) {
   uint16_t wrl = ntohs ( * (uint16_t*) msg);
   assert (wrl < length-1);
   uint16_t tpal = ntohs ( * (uint16_t*) (msg+wrl+2));
   char *pa = msg+wrl+4;
   assert (wrl + tpal < length-3);
   char *nlri = msg+wrl+tpal+4;
   uint16_t nlril = length - wrl - tpal - 4;
   //fprintf(stderr, "%d: BGP Update: withdrawn length =  %d, path attributes length = %d , NLRI length = %d\n",pid,wrl,tpal,nlril);
   int wc,uc;
   if ( wrl > 0 )
        wc = spnlri(msg,wrl);
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
   unsigned int pl;
   unsigned char msgtype;

   header = bufferedRead(sb,19);
   if (0 == header ) {
      fprintf(stderr, "%d: end of stream\n",pid);
      return 0;
   } else if (!isMarker(header)) {
      die("Failed to find BGP marker in msg header from peer");
            return -1;
   } else {
      pl = ( header[16] << 8 ) + ( header[17] ) - 19 ;
      msgtype = header[18];
      if (0 < pl) {
         payload=bufferedRead(sb,pl);
         if (0 == payload) {
            fprintf(stderr, "%d: end of stream\n",pid);
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
   } else {
      // fprintf(stderr,"+");
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

void session(int sock, int fd1 , int fd2) {
  int i,msgtype;
  struct sockbuf sb;

  msgcount = 0;
  update_count = 0;
  update_nlri_count = 0;
  update_withdrawn_count = 0;

  setsockopt( sock, IPPROTO_TCP, TCP_NODELAY, (void *)&i, sizeof(i));
  lseek(fd1,0,0);
  lseek(fd2,0,0);
  bufferInit(&sb,sock,BUFFSIZE);

  (0 < sendfile(sock, fd1, 0, 0x7ffff000)) || die("Failed to send fd1 to peer");

  msgtype=getBGPMessage (&sb); // this is expected to be an Open
  report(1,msgtype);

  (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");

  msgtype=getBGPMessage (&sb); // this is expected to be a Keepalive
  report(4,msgtype);

  (0 < sendfile(sock, fd2, 0, 0x7ffff000)) || die("Failed to send fd2 to peer");

  fprintf(stderr, "%d: session: sendfile complete\n",pid);

  do {
        msgtype = getBGPMessage (&sb); // keepalive or updates from now on
        if (msgtype==3){
            fprintf(stderr, "%d: session: got Notification\n",pid);
            break;
        } else if (msgtype==4){
            (0 < send(sock, keepalive, 19, 0)) || die("Failed to send keepalive to peer");
        } else
            report(2,msgtype);
  } while (msgtype>0);
  close(sock);
  // bufferClose();
  fprintf(stderr, "%d: session exit, msg cnt = %d, updates = %d, NLRIs = %d, withdrawn = %d\n",pid,msgcount,update_count,update_nlri_count,update_withdrawn_count);
  msgcount = 0;
}

int main(int argc, char *argv[]) {
  int serversock, peersock, fd1,fd2;
  struct sockaddr_in peeraddr;

  pid = getpid();
  fprintf(stderr, "%d: bgpc\n",pid);
  if (3 > argc) {
      fprintf(stderr, "USAGE: bgpc <open_message_file> <update_message_file> {IP address}\n");
      exit(1);
  }
  if ((fd1 = open(argv[1],O_RDONLY)) < 0) {
    die("Failed to open BGP Open message file");
  }

  if ((fd2 = open(argv[2],O_RDONLY)) < 0) {
    die("Failed to open BGP Update message file");
  }

  if (3 == argc) { // server mode.....

    memset(&peeraddr, 0, SOCKADDRSZ );
    peeraddr.sin_family = AF_INET;
    peeraddr.sin_addr.s_addr = htonl(INADDR_ANY);   // local server addr - wildcard - could be a specific interface
    peeraddr.sin_port = htons(179);       // BGP server port

    if ((serversock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
      die("Failed to create socket");
    }

    int reuse = 1;
    if (setsockopt(serversock, SOL_SOCKET, SO_REUSEADDR, (const char*)&reuse, sizeof(reuse)) < 0)
      die("Failed to set server socket option SO_REUSEADDR");

    if (setsockopt(serversock, SOL_SOCKET, SO_REUSEPORT, (const char*)&reuse, sizeof(reuse)) < 0)
      die("Failed to set server socket option SO_REUSEPORT");

    if (bind(serversock, (struct sockaddr *) &peeraddr, SOCKADDRSZ ) < 0) {
      die("Failed to bind the server socket");
    }

    if (listen(serversock, MAXPENDING) < 0) {
      die("Failed to listen on server socket");
    }

    while (1) {
      unsigned int addrsize;
      fprintf(stderr, "%d: waiting for connection\n",pid);
      if ((peersock = accept(serversock, (struct sockaddr *) &peeraddr, &addrsize )) < 0) {
        die("Failed to accept peer connection");
      }
      if ( addrsize != SOCKADDRSZ || AF_INET != peeraddr.sin_family) {
        die("bad sockaddr");
      }
      fprintf(stderr, "%d: Peer connected: %s\n",pid, inet_ntoa(peeraddr.sin_addr));
      session(peersock,fd1,fd2);
    }
  } else { // client mode
      fprintf(stderr, "%d: Connecting to: %s\n",pid, argv[3]);
      memset(&peeraddr, 0, SOCKADDRSZ );
      peeraddr.sin_family = AF_INET;
      peeraddr.sin_addr.s_addr = inet_addr(argv[3]);
      peeraddr.sin_port = htons(179);
      if ((peersock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
        die("Failed to create socket");
      } else if (connect(peersock, (struct sockaddr *) &peeraddr, SOCKADDRSZ ) < 0) {
        die("Failed to connect with peer");
      } else {
          fprintf(stderr, "%d: Peer connected: %s\n",pid, argv[3]);
          session(peersock,fd1,fd2);
      }
  }
}
