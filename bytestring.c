#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>
#include <sys/socket.h>
#include "libutil.h"

void sendbs(int sock, struct bytestring msg ) {
   assert (0 < send(sock, msg.data, msg.length, 0));
};

struct bytestring empty = { 0 , 0 };

struct bytestring EOS = { 0xffff , 0 };

char * hexbytestring ( struct bytestring bs ) {
    return toHex (bs.data,bs.length);
};

struct bytestring concatbytestring(struct bytestring bs0 , ...) {

    if (bs0.length == 0xffff)
        return EOS;
    int length = bs0.length;
    // DEBUG // printf(" 0: %d/%d\n",length,length);
    va_list ap0;
    va_start(ap0,bs0);
    struct bytestring bs = va_arg(ap0,struct bytestring);
    int i = 1;
    while (0xffff != bs.length) {
        length += bs.length;
        // DEBUG // printf("%2d: %d/%d\n",i,bs.length,length);
        bs = va_arg(ap0,struct bytestring);
        i++;
    };
    va_end(ap0);
    // DEBUG // printf("%d strings, total length %d\n",i,length);

    va_list ap1;
    va_start(ap1,bs0);
    bs = bs0;
    int j = 0;
    char * buf = malloc(length);
    char * next = buf;
    do {
       if (bs.length != 0) {
          next = mempcpy(next,bs.data,bs.length);
       };
       j++;
       bs = va_arg(ap1,struct bytestring);
    } while (0xffff != bs.length);
    va_end(ap1);
    return (struct bytestring) {length,buf};
};
