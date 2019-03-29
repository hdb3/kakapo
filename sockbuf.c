

// sockbuf.c

#include <stdio.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "sockbuf.h"
#include "util.h"

void bufferInit (struct sockbuf *sb, int _sock, int size) {
    sb->sock = _sock;
    sb->start = 0;
    sb->count = 0;
    sb->top = size;
    sb->threshold = size * 3 / 4;
    sb->base = malloc(size);
};

unsigned char * bufferedRead (struct sockbuf *sb, int rc) {

    int sockRead;

    // first recv from socket until we can satisfy the request
    // this may require a shuffle operation before the first read

    assert(rc<sb->top);

    fprintf(stderr,"sockbuf.c: rc: %d\n",rc);
    fprintf(stderr,"sockbuf.c: step 0 : %d / %d\n",sb->start,sb->count);
    printHex(stderr,sb->base+sb->start,sb->count);

    if ((rc > sb->count) && ( sb->start + sb->count > sb->threshold)) {
        // reshuffle
        // //fprintf(stderr,"sockbuf.c: reshuffle\n");
        // //char * to = sb->base;
        // //char * from = sb->base+sb->start;
        // //int i=0;
        // //int c=sb->count;
        // //for ( ; i<c ; i++)
            // //to[i] = from[i];
        memmove(sb->base, sb->base + sb->start, sb->count);
        sb->start = 0;
    }
    fprintf(stderr,"sockbuf.c: step 1 : %d / %d\n",sb->start,sb->count);
    printHex(stderr,sb->base+sb->start,sb->count);
    while ( rc > sb->count ) {
        //fprintf(stderr,"sockbuf.c: waiting on recv, request=%d, available=%d\n",rc,sb->count);
        sockRead = recv( sb->sock, sb->base + sb->start, sb->top - sb->start - sb->count, 0 );
        // if zero or worse, die...
        if ( sockRead < 0 ) {
            perror(0);
            return 0;
        } else if ( sockRead == 0 )
            return 0;
        else {
            sb->count = sb->count + sockRead;
            fprintf(stderr,"sockbuf.c: recv got : %d\n",sockRead);
        }
    }
    fprintf(stderr,"sockbuf.c: step 2 : %d / %d\n",sb->start,sb->count);
    printHex(stderr,sb->base+sb->start,sb->count);
    // if we get here then we know there is enough in the buffer to satisfy the request
    sb->count = sb->count - rc;
    int tmp = sb->start;
    sb->start = sb->start + rc;
    fprintf(stderr,"sockbuf.c: step 3 : %d / %d\n",sb->start,sb->count);
    printHex(stderr,sb->base+sb->start,sb->count);
    printHex(stderr,sb->base+sb->start,rc);
    return ( sb->base + tmp );
}
