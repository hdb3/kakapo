

#include <stdio.h>
#include <stdint.h>
#include "util.h"

// test cases for the hexXX int to hex string functions now in util.c
/*

char * hex8(uint8_t n) {
    char * s;
    asprintf(&s,"%02hhX",n);
    return s;
};

char * hex16(uint16_t n) {
    char * s;
    asprintf(&s,"%04hX",n);
    return s;
};

char * hex32(uint32_t n) {
    char * s;
    asprintf(&s,"%08X",n);
    return s;
};

char * hex64(uint64_t n) {
    char * s;
    asprintf(&s,"%016LX",n);
    return s;
};
*/

int main(int argc, char **argv) {

    printf("%0X\n",0x1deadbeef);
    printf("%04X\n",0x1deadbeef);
    printf("%04lX\n",0x1deadbeef);
    printf("%04hX\n",0x1deadbeef);
    printf("%4hX\n",0x1dead00ef);
    printf("%04hX\n",0x1dead00ef);
    printf("%02hhX\n",0x1dead00ef);

    char * s8 = hex8(0x1dead00ef);
    char * s16 = hex16(0x1dead00ef);
    char * s32 = hex32(0x1dead00ef);
    char * s64 = hex64(0x1dead00ef);

    char * s = concat(s8,s16,s32,s64,NULL);

    printf("%s\n",hex16(1968));
    printf("%s %s %s %s\n",s8,s16,s32,s64);
    printf("%s\n",s);
};
