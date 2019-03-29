
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

unsigned int start,count,top,threshold;
unsigned char *base;

bufferInit (int size) {
    start = 0;
    count = 0;
    top = size;
    threshold = size * 3 / 4;
    base = malloc(size);
}

unsigned char * bufferedRead (int rc) {

    if ( rc < count ) {
        count = count - rc;
        int tmp = start;
        start = start + rc;
        return ( base + start );
    } else {
        if (start + count > threshold) {
            // reshoffle
        }
        sockRead = read(sock,base+start,top-start);
        // if zero or worse, die...
        count = count + sockRead;
//
