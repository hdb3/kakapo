
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

unsigned int sock,start,count,top,threshold;
unsigned char *base;

void bufferInit (int _sock, int size) {
    sock = _sock;
    start = 0;
    count = 0;
    top = size;
    threshold = size * 3 / 4;
    base = malloc(size);
}

unsigned char * bufferedRead (int rc) {
    int sockRead;

    // first recv from socket until we can satisfy the request
    // this may require a shuffle operation before the first read

    if ((rc > count) && ( start + count > threshold)) {
        // reshuffle
        memmove(base,base+start,count);
        start = 0;
    }
    while ( rc > count ) {
        sockRead = recv( sock, base+start, top-start, 0 );
        // if zero or worse, die...
        if ( sockRead < 0 ) {
            perror(0);
            return 0;
        } else if ( sockRead == 0 )
            return 0;
        else
            count = count + sockRead;
    }
    // if we get here then we know there is enough in the buffer to satisfy the request
    count = count - rc;
    int tmp = start;
    start = start + rc;
    return ( base + tmp );
}
//
