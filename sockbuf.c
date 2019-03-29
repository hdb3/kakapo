

// sockbuf.c
#include "sockbuf.h"

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

    if ((rc > sb->count) && ( sb->start + sb->count > sb->threshold)) {
        // reshuffle
        memmove(sb->base, sb->base + sb->start, sb->count);
        sb->start = 0;
    }
    while ( rc > sb->count ) {
        sockRead = recv( sb->sock, sb->base + sb->start, sb->top - sb->start, 0 );
        // if zero or worse, die...
        if ( sockRead < 0 ) {
            perror(0);
            return 0;
        } else if ( sockRead == 0 )
            return 0;
        else
            sb->count = sb->count + sockRead;
    }
    // if we get here then we know there is enough in the buffer to satisfy the request
    sb->count = sb->count - rc;
    int tmp = sb->start;
    sb->start = sb->start + rc;
    return ( sb->base + tmp );
}
