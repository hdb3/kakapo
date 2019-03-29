
/*

Variables
* base - buffer address - fixed during execution
* start - the current read cursor - reset to base after a reset or when a read clears count to zero - incremented on reads
* count - effectively the current write cursor, as an offset beyond start.  Incremented on writes, decremented on reads
* top and threshold - the buffer limits - top used to calculate the size of a socket read request - threshold used to decide when a reset should occur
Algorithm
Read ->
* Check if the request can be satisfied from the buffer (req cont =< count) - if it can exit via ‘satisfy’
* Check if there is a need to reset the buffer - if so, do buffer reset before next stage
* Execute the socket read, return to the top (except if zero return in which case it is a fail)

*/

struct sockbuf {
    int sock,start,count,top,threshold;
    char *base;
};

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
