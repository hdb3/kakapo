
// compile wrapper for sockbuf

#include "sockbuf.h"

int main(int argc, char *argv[]) {
    char *buf;
    struct sockbuf sb;
    bufferInit(&sb,99,100);
    do {
        buf = bufferedRead(&sb,10);
    } while (0 != buf);
    return 0;
}
