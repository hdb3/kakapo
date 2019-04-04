

// sockbuf.h

struct sockbuf {
    int sock,timeout;
    unsigned int start,count,top,threshold;
    char *base;
};

void bufferInit (struct sockbuf *sb, int sock, int size, int timeout);
char * bufferedRead (struct sockbuf *sb, int rc);
