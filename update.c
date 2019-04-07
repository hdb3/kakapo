#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <byteswap.h>
#include <assert.h>
#include <byteswap.h>
#include "bytestring.h"
#include "update.h"

struct bytestring updatehdr (uint16_t length ) {
    static char b [19] = { 0xff , 0xff ,0xff ,0xff ,0xff ,0xff ,0xff ,0xff ,
                           0xff , 0xff ,0xff ,0xff ,0xff ,0xff ,0xff ,0xff ,
                           0x00 , 0x00 , 0x2 };
   uint16_t * lptr = (uint16_t *) (16 + b);
   *lptr = __bswap_16 (length+19);
   return (struct bytestring) { 19 , b};
};

struct bytestring update (struct bytestring nlri, struct bytestring withdrawn, struct bytestring pathattributes) {

   uint16_t
   payloadlength = nlri.length + withdrawn.length + pathattributes.length;

   struct
   bytestring hdr = updatehdr ( payloadlength );

   uint16_t
   messagelength = payloadlength + hdr.length; // yes hdr.length IS always 19...

   char *
   buf = malloc ( messagelength );

   char *
   next = mempcpy (buf , hdr.data , hdr.length);
   *((uint16_t*) next) = __bswap_16 ( withdrawn.length ); next+=2;
   next = mempcpy (next , withdrawn.data , withdrawn.length);
   *((uint16_t*) next) = __bswap_16 ( pathattributes.length ); next+=2;
   next = mempcpy (next , pathattributes.data , pathattributes.length);
   next = mempcpy (next , nlri.data , nlri.length);
   return (struct bytestring) { messagelength , buf};
};

