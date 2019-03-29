
// bgps.c

#include <stdio.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include "sockbuf.h"

#define MAXPENDING 5    // Max connection requests
#define BUFFSIZE 0x10000
#define SOCKADDRSZ (sizeof (struct sockaddr_in))
#define VERBOSE (1)

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

int getBGPMessage (struct sockbuf *sb) {
   char *header;
   char *payload;
   int received;
   unsigned int pl;
   unsigned char msgtype;

   header = bufferedRead(sb,19);
   if (0 == header ) {
      fprintf(stderr, "end of stream\n");
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
            fprintf(stderr, "end of stream\n");
            return 0;
         }
     } else
         payload = 0;
   }
   msgcount++;
   if VERBOSE {
      unsigned char *hex = toHex (payload,pl) ;
      fprintf(stderr, "BGP msg type %s length %d received [%s]\n", showtype(msgtype), pl , hex);
      free(hex);
   } else {
      fprintf(stderr,"+");
   }
   return msgtype;
}

void simple(int sock, FILE * f ) {
  char *buf;
  struct sockbuf sb;
  bufferInit(&sb,sock,0x100000);
  buf = bufferedRead(&sb,10);
  while (NULL != buf) {
        printHex(stderr,buf,10);
        fwrite(buf,1,10,f);
        buf = bufferedRead(&sb,10);
  }
}

void session(int sock, FILE * f ) {
  int msgtype;
  struct sockbuf sb;
  bufferInit(&sb,sock,0x100000);

  msgtype=getBGPMessage (&sb); // this is expected to be an Open
  if (msgtype==1)
      fprintf(stderr, "session: got Open\n");
  else
      fprintf(stderr, "session: expected Open, got %s (%d)\n",showtype(msgtype),msgtype);

  msgtype=getBGPMessage (&sb); // this is expected to be a Keepalive
  if (msgtype==4)
      fprintf(stderr, "session: got Keepalive\n");
  else
      fprintf(stderr, "session: expected Keepalive, got %s (%d)\n",showtype(msgtype),msgtype);

  do {
        msgtype = getBGPMessage (&sb); // keepalive or updates from now on
        if (msgtype==4)
            fprintf(stderr, "session: got Keepalive\n");
        else if (msgtype==2)
            fprintf(stderr, "session: got Update\n");
        else
            fprintf(stderr, "session: expected Keepalive, got %s (%d)\n",showtype(msgtype),msgtype);

  } while (msgtype>0);
  close(sock);
  // bufferClose();
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
      if ((f = fopen(argv[1],"w")) < 0) {
         die("Failed to open outputfile");
      }
      //simple(peersock,f);
      session(peersock,f);
      fclose(f);
   }
}
