// sockbuf.h

#ifndef __SOCKBUF_H
#define __SOCKBUF_H
struct sockbuf {
    int sock,timeout;
    struct timespec timestamp;
    unsigned int start,count,top,threshold;
    char *base;
};

void bufferInit (struct sockbuf *sb, int sock, int size, int timeout);
char * bufferedRead (struct sockbuf *sb, int rc);
long long int lastrecvtime(struct sockbuf *sb);
#endif
