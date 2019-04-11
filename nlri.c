#include "bytestring.h"
#include <assert.h>
#include <byteswap.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

struct bytestring nlris(uint32_t ipstart, uint8_t length, int count, int seq) {

  uint8_t chunksize;
  if (length == 0)
    chunksize = 1;
  else if (length < 9)
    chunksize = 2;
  else if (length < 17)
    chunksize = 3;
  else if (length < 25)
    chunksize = 4;
  else if (length < 33)
    chunksize = 5;
  else
    assert(0);
  int bufsize = chunksize * count;
  char *buf = malloc(bufsize);
  char *next = buf;
  uint32_t ip = __bswap_32(ipstart);
  uint32_t increment = 1 << (32 - length);
  ip += seq * count * increment; // generate sequence of NLRIs
  uint32_t x[2];
  uint8_t *lptr = 3 + (uint8_t *)x;
  uint32_t *addrptr = x + 1;
  *lptr = length;
  char *loc = 3 + (char *)x;
  for (int i = 0; i < count; i++) {
    *addrptr = __bswap_32(ip);
    memcpy(next, loc, chunksize);
    ip += increment;
    next += chunksize;
  };
  return (struct bytestring){bufsize, buf};
};
