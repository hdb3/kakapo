
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
#include "sockbuf.h"

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

unsigned char *toHex (unsigned char *buf, unsigned int l) {

  unsigned char     hex_str[]= "0123456789abcdef";
  unsigned int      i;
  unsigned char *result;

  if (!(result = (unsigned char *)malloc(l * 2 + 1)))
    return (NULL);

  (result)[l * 2] = 0;

  if (!l)
    return (NULL);

  for (i = 0; i < l; i++)
    {
      (result)[i * 2 + 0] = hex_str[(buf[i] >> 4) & 0x0F];
      (result)[i * 2 + 1] = hex_str[(buf[i]     ) & 0x0F];
    }
  return (result);
}

void printHex ( FILE * fd, unsigned char *buf, unsigned int l) {
      unsigned char *hex = toHex (buf,l) ;
      fprintf(fd, "[%s]\n",hex);
      free(hex);
}

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

  do {
        msgtype = getBGPMessage (&sb); // keepalive or updates from now on
        report(2,msgtype);
        if (msgtype==3){
            fprintf(stderr, "%d: session: got Notification\n",pid);
            break;
        }
  } while (msgtype>0);
  close(sock);
  // bufferClose();
  fprintf(stderr, "%d: session exit, msg cnt = %d\n",pid,msgcount);
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
