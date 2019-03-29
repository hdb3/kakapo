

// sockbuf.h


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

struct sockbuf {
    int sock,start,count,top,threshold;
    char *base;
};

void bufferInit (struct sockbuf *sb, int _sock, int size);
unsigned char * bufferedRead (struct sockbuf *sb, int rc);
