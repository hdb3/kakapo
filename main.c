

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
#include "bufferedread.c"

int main(int argc, char *argv[]) {
    char *buf;
    struct sockbuf sb;
    bufferInit(&sb,99,100);
    do {
        buf = bufferedRead(&sb,10);
    } while (NULL != buf);
    return 0;
}
