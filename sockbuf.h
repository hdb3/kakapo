

// sockbuf.h

struct sockbuf {
    int sock,start,count,top,threshold;
    char *base;
};

void bufferInit (struct sockbuf *sb, int _sock, int size);
unsigned char * bufferedRead (struct sockbuf *sb, int rc);