#include "bytestring.h"
#include "kakapo.h"
#include <arpa/inet.h>
#include <assert.h>
#include <byteswap.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

static char *s;
char *showprefix(struct prefix pfx) {
  int tmp = asprintf(&s, "%s/%d", fromHostAddress(pfx.ip), pfx.length);
  return s;
};
int cmp_prefix(struct prefix *pfxa, struct prefix *pfxb) {
  return (pfxa->ip == pfxb->ip) && (pfxa->length == pfxb->length);
};

static char nlribuffer[65535]; // over large, but withdraw can make BGP Update at large sizes
                               // and busting the 4096 limit is allowed in some implmentations
                               // so 2^16 is only safe value
                               // NOTE: single thread only
                               //       replaces a previous malloc based version

static struct prefix prefix_list[32768]; // over large to match the logic used to size nlribuffer
                                         // used as the return value when a prefix list is assembled by get_prefix_list
                                         // In every case th eused space will correspond to GROUPSIZE entries unless another use emerges

struct prefix *get_prefix_list(uint32_t ipstart, uint8_t length, int count, int seq) {
  uint32_t i;
  uint32_t ip = __bswap_32(ipstart);
  uint32_t increment = 1 << (32 - length);
  for (i = 0; i < count; i++) {
    prefix_list[i] = (struct prefix){__bswap_32(ip + (seq + i) * increment), length};
  };
  return prefix_list;
};

struct bytestring nlris(uint32_t ipstart, uint8_t length, int count, int seq) {

  uint8_t chunksize = 1 + (length + 7) / 8;
  int bufsize = chunksize * count;
  char *buf = nlribuffer;
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
  while (offset < nlris.length) {
    struct prefix pfx2 = get_prefix_nlri(nlris.data + offset);
    if (pfx.length == pfx2.length && pfx.ip == pfx2.ip)
      return 1;
    length = (uint8_t) * (nlris.data + offset);
    chunksize = 1 + (length + 7) / 8;
    offset += chunksize;
  };
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
    chunksize = 1 + (length + 7) / 8;
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
    chunksize = 1 + (length + 7) / 8;
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
