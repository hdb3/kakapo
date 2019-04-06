#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <malloc.h>
#include "util.h"

struct bytestring { uint16_t length ; char* data; };
//typedef struct { uint16_t length ; char* data; } bytestring;

///struct bytestring EOS  = (struct bytestring) { 0xffff , 0 };
///struct bytestring BS0  = (struct bytestring) { 0 , 0 };
///struct bytestring BS1  = (struct bytestring) { 1 , { 1 } };
///struct bytestring BS10  = (struct bytestring) { 10 , { 0,1,2,3,4,5,6,7,8,9 } };
struct bytestring EOS  = { 0xffff , 0 };
struct bytestring BS0  = { 0 , 0 };
struct bytestring BS1  = { 1 , "\x1" };
struct bytestring BS10  = { 10 , "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09" };
struct bytestring BS10a  = { 10 , "\x09\x08\x07\x06\x05\x04\x03\x02\x01\x00" };

char * hexbytestring ( struct bytestring bs );

struct bytestring concatbytestring(struct bytestring bs0 , ...);

char *toHex (char *buf, int l);

char * hexbytestring ( struct bytestring bs ) {
    return toHex (bs.data,bs.length);
};

struct bytestring concatbytestring(struct bytestring bs0 , ...) {

    if (bs0.length == 0xffff)
        return EOS;
    int length = bs0.length;
    printf(" 0: %d/%d\n",length,length);
    va_list ap0;
    va_start(ap0,bs0);
    struct bytestring bs = va_arg(ap0,struct bytestring);
    int i = 1;
    while (0xffff != bs.length) {
        length += bs.length;
        printf("%2d: %d/%d\n",i,bs.length,length);
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
    return (struct bytestring) {length,buf};
};

int main(int argc, char** argv) {
    printf("BS0 %s\n",hexbytestring(BS0));
    printf("BS1 %s\n",hexbytestring(BS1));
    printf("BS10 %s\n",hexbytestring(BS10));
    struct bytestring cbs = concatbytestring(BS0,BS1,BS10,EOS);
    printf("concat result %s\n",hexbytestring(cbs));
    printf("concat BS10 ++ BS10a %s\n",hexbytestring(concatbytestring(BS10,BS10a,EOS)));
};
