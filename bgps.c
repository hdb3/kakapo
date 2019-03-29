

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
#include "bufferedread.c"

#define MAXPENDING 5    // Max connection requests
#define BUFFSIZE 0x10000
#define SOCKADDRSZ (sizeof (struct sockaddr_in))
#define VERBOSE (0)
#define FAST (0)

int die(char *mess) { perror(mess); exit(1); }
unsigned char keepalive [19]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 19, 4 };
unsigned char marker [16]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
int isMarker (const unsigned char *buf) {
   return ( 0 == memcmp(buf,marker,16));
}

int msgcount = 0;

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

int getBGPMessage () {
   char *header;
   char *payload;
   int received;
   unsigned int pl;
   unsigned char msgtype;

   header = bufferedRead(19);
   if (0 == header ) {
      fprintf(stderr, "end of stream\n");
      return 0;
   } else if (!isMarker(header)) {
      die("Failed to find BGP marker in msg header from peer");
   } else {
      pl = ( header[16] << 8 ) + ( header[17] ) - 19 ;
      msgtype = header[18];
      if (0 < pl) {
         payload=bufferedRead(pl);
         if (0 == payload) {
            fprintf(stderr, "end of stream\n");
            return 0;
         }
     } else
         payload = 0;
   }
   if VERBOSE {
      unsigned char *hex = toHex (payload,pl) ;
      fprintf(stderr, "BGP msg type %s length %d received [%s]\n", showtype(msgtype), pl , hex);
      free(hex);
   } else
      msgcount++;
   //fprintf(stderr,"+");
   return 1;
}

void simple(int sock, FILE * f ) {
  char *buf;
  bufferInit(sock,0x100000);
  do {
        buf = bufferedRead(10);
        fwrite(buf,1,10,f);
  } while (NULL != buf);
}

void session(int sock, FILE * f ) {
  int res;
  bufferInit(sock,0x100000);
  getBGPMessage (); // this is expected to be an Open

  getBGPMessage (); // this is expected to be a Keepalive

  do {
        res = getBGPMessage (); // keepalive or updates from now on
  } while (res>0);
  close(sock);
  fprintf(stderr, "session exit, msg cnt = %d\n",msgcount);
  msgcount = 0;
}

int main(int argc, char *argv[]) {
   int serversock, peersock;
   struct sockaddr_in peeraddr;
   FILE *f;

   fprintf(stderr, "bgps\n");

   if (2 > argc) {
      fprintf(stderr, "USAGE: bgpc <dump_file>\n");
      exit(1);
   }

   if ((f = fopen(argv[1],"w")) < 0) {
      die("Failed to open outputfile");
   }

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
      fprintf(stderr, "waiting for connection\n");
      if ((peersock = accept(serversock, (struct sockaddr *) &peeraddr, &addrsize )) < 0) {
         die("Failed to accept peer connection");
      }
      if ( addrsize != SOCKADDRSZ || AF_INET != peeraddr.sin_family) {
         die("bad sockaddr");
      }
      fprintf(stderr, "Peer connected: %s\n", inet_ntoa(peeraddr.sin_addr));
      session(peersock,f);
   }
}
