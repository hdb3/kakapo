#define _GNU_SOURCE
#include "bytestring.h"
#include "kakapo.h"
#include <arpa/inet.h>
#include <assert.h>
#include <byteswap.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

char *showprefix(struct prefix pfx) {
  char *s;
  int tmp = asprintf(&s, "%s/%d", fromHostAddress(pfx.ip), pfx.length);
  return s;
};

struct bytestring nlris(uint32_t ipstart, uint8_t length, int count, int seq) {

  uint8_t chunksize = 2 + (length - 1) / 8;
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
  int i;
  for (i = 0; i < count; i++) {
    *addrptr = __bswap_32(ip);
    memcpy(next, loc, chunksize);
    ip += increment;
    next += chunksize;
  };
  return (struct bytestring){bufsize, buf};
};

struct prefix get_prefix_nlri(char *nlri) {
  uint8_t length = (uint8_t)*nlri;
  uint8_t chunksize = 1 + (length - 1) / 8;
  uint32_t acc = 0;
  uint8_t i, b;
  // accumulate with shift the most significant bytes
  for (i = 0; i < chunksize; i++)
    acc = (acc << 8) + (uint8_t) * (nlri + 1 + i);
  // apply the remaining shift and byteswap for canonical form
  acc = __bswap_32(acc) << (8 * (4 - chunksize));
  return (struct prefix){acc, length};
};

int nlri_member(struct bytestring nlris, struct prefix pfx) {
  uint8_t length, chunksize;
  int offset = 0;
  //printf("nlri_member? %s\n",showprefix(pfx));
  while (offset < nlris.length) {
    //printf("offset: %d\n",offset);
    struct prefix pfx2 = get_prefix_nlri(nlris.data + offset);
    //printf("nlri_member: %s\n",showprefix(pfx2));
    if (pfx.length == pfx2.length && pfx.ip == pfx2.ip)
      return 1;
    length = (uint8_t) * (nlris.data + offset);
    chunksize = 2 + (length - 1) / 8;
    //printf("offset=%d length=%d chunksize=%d\n",offset,length,chunksize);
    offset += chunksize;
  };
  //printf("nlri_member fail\n");
  return 0;
};

int nlri_list(struct bytestring nlris, struct prefix **pfxs) {
  uint8_t length, chunksize;
  int offset = 0;
  int pfx_count = 0;
  *pfxs = calloc(2048, sizeof(struct prefix));
  while (offset < nlris.length) {
    length = (uint8_t) * (nlris.data + offset);
    (*pfxs)[pfx_count++] = get_prefix_nlri(nlris.data + offset);
    chunksize = 2 + (length - 1) / 8;
    offset += chunksize;
  };
  return pfx_count;
};

int nlri_count(struct bytestring nlris) {
  uint8_t length, chunksize;
  int offset = 0;
  int pfx_count = 0;
  while (offset < nlris.length) {
    length = (uint8_t) * (nlris.data + offset);
    pfx_count++;
    chunksize = 2 + (length - 1) / 8;
    offset += chunksize;
  };
  return pfx_count;
};

void *showprefixes(struct bytestring nlris) {
  struct prefix *pfxs;
  int i;
  int count = nlri_list(nlris, &pfxs);
  printf("%d prefixes\n", count);
  for (i = 0; i < count; i++) {
    printf("%s\n", showprefix(pfxs[i]));
  };
};
