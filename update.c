#include <stdio.h>
#include <stdint.h>
#include <byteswap.h>
#include "bytestring.h"
#include "update.h"

#define Optional (0x80)
#define Transitive (0x40)
#define Partial (0x20)
#define ExtendedLength (0x10)
#define ORIGIN 1
#define AS_PATH 2
#define NEXT_HOP 3
#define MULTI_EXIT_DISC 4
#define LOCAL_PREF 5
#define AS_SEQUENCE 2
#define INCOMPLETE 2

//char * paOrigin, paLocalPref, paMED;
//char *paNextHop (uint32_t nexthop);
//char *paASPATH(uint32_t *asn);


struct bytestring pa2bytestring (char* pa) {
   if ((*pa) & ExtendedLength)
      return (struct bytestring) { 4 + pa[3] + (pa[2] *256) , pa };
   else
      return (struct bytestring) { 3 + pa[2] , pa };
};

char b0(uint32_t w) {
    return ((uint32_t) 0xff) & (w >> 24);
};

char b1(uint32_t w) {
    return ((uint32_t) 0xff) & (w >> 16);
};

char b2(uint32_t w) {
    return ((uint32_t) 0xff) & (w >> 8);
};

char b3(uint32_t w) {
    return ((uint32_t) 0xff) & w ;
};

char paOrigin [] = {  Transitive , ORIGIN , 1 , INCOMPLETE };
char paLocalPref [] = {  Transitive , LOCAL_PREF , 1 , 100 };
char paMED [] = {  Optional , MULTI_EXIT_DISC , 1 , 100 };

char *paNextHop (uint32_t nexthop) {
    static char b [] = { 0 , NEXT_HOP , 4 , 0, 0, 0, 0 };
    //static char b [] = { 0 , NEXT_HOP , 4 , b0(nexthop) ,  b1(nexthop) , b2(nexthop) , b3(nexthop) };
    memcpy ( b+3 , &nexthop,4);
    return  b;
};

char *paASPATH(uint32_t *asn) {
    //static char b [] = { ExtendedLength , AS_PATH , 0, 2 , AS_SEQUENCE , 0 };
    //return b;
    static char b [4096] = { ExtendedLength , AS_PATH , 0, 2 , AS_SEQUENCE , 0 };
    uint8_t i = 0;
    uint32_t *from = asn;
    uint32_t *to = (uint32_t*) (b+6);
    while (0 != *from) {
        //printf("ASN:%d\n",*p);
        memcpy (to,from,4);
        i++;  from++; to++;
    };
    memcpy (b + 5 , (char*) (&i) , 1);
    //i << 2; i += 2; i = __bswap_16(i);
    uint16_t j = __bswap_16(2 + (i << 2));
    memcpy (b + 2 , (char*) (&j) , 2);
    return b;
    // return { ExtendedLength , AS_PATH , 6 , AS_SEQUENCE , 0, 0, b0(asn[0]) ,  b1(asn[0]) , b2(asn[0]) , b3(asn[0]) } ;
};
