#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <byteswap.h>
#include <assert.h>
#include "bytestring.h"


struct bytestring nlris (uint32_t ipstart , uint8_t length, int count) {

   uint8_t chunksize;
   if ( length == 0 )
      chunksize = 1;
   else if ( length < 9 )
      chunksize = 2;
   else if ( length < 17 )
      chunksize = 3;
   else if ( length < 25 )
      chunksize = 4;
   else if ( length < 33 )
      chunksize = 5;
   else
      assert (0);
   int bufsize = chunksize * count;
   char * buf = malloc(bufsize);
   char * next = buf;
   uint32_t ip = ipstart;
   uint32_t increment = 1 << (32-length);
   uint32_t x [2];
   x[0] = length; 
   char * loc = 3 + (char *) x;
   for (int i = 0 ; i < count ; i++ ) {
      x[1] = ip;
      memcpy( next , loc , chunksize );
      ip += increment;
      next += chunksize;
   };
   return (struct bytestring) { bufsize , buf };
};
