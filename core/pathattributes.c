#define _GNU_SOURCE
#include "pathattributes.h"
#include "bytestring.h"
#include <assert.h>
#include <byteswap.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

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

static inline uint16_t palength(char *pa) {
  if ((*pa) & ExtendedLength)
    return 4 + pa[3] + (pa[2] * 256);
  else
    return 3 + pa[2];
};

static char attrbuffer[65535]; // over large......
                               // NOTE: single thread only
                               //       replaces a previous malloc based version

struct bytestring pas2bytestring(char *pa, ...) {
  char *pas[16];
  uint16_t lengths[16];

  uint16_t length = 0;
  int pax = 0;

  lengths[pax] = palength(pa);
  length += lengths[pax];
  pas[pax] = pa;
  pax++;

  va_list ap;
  va_start(ap, pa);
  char *pav = va_arg(ap, char *);
  while (0 != pav) {
    uint16_t l = palength(pav);
    if (l > 0) {
      lengths[pax] = l;
      length += l;
      pas[pax] = pav;
      pax++;
    };
    pav = va_arg(ap, char *);
  };
  va_end(ap);
  char *buf = attrbuffer;
  char *next = buf;
  int i;
  for (i = 0; i < pax; i++)
    next = mempcpy(next, pas[i], lengths[i]);
  return (struct bytestring){length, buf};
};

struct bytestring pa2bytestring(char *pa) {
  return (struct bytestring){palength(pa), pa};
};

char paOrigin[] = {Transitive, ORIGIN, 1, INCOMPLETE};
// char paMED[] = {Optional, MULTI_EXIT_DISC, 4, 0, 0, 0, 100};
char *paMED(uint32_t med) {
  static char b[] = {Optional, MULTI_EXIT_DISC, 4, 0, 0, 0, 0};
  *((uint32_t *)(b + 3)) = __bswap_32(med);
  return b;
};

char *paLocalPref(uint32_t localpref) {
  static char b[] = {Transitive, LOCAL_PREF, 4, 0, 0, 0, 0};
  *((uint32_t *)(b + 3)) = __bswap_32(localpref);
  return b;
};

char *paNextHop(uint32_t nexthop) {
  static char b[] = {Transitive, NEXT_HOP, 4, 0, 0, 0, 0};
  *((uint32_t *)(b + 3)) = nexthop;
  return b;
};

char *paASPATH(uint32_t *asn) {
  static char b[4096] = {
      ExtendedLength | Transitive, AS_PATH, 0, 2, AS_SEQUENCE, 0};
  uint8_t i = 0;
  uint32_t *from = asn;
  uint32_t *to = (uint32_t *)(b + 6);
  while (0 != *from) {
    *to = __bswap_32(*from);
    i++;
    from++;
    to++;
  };
  memcpy(b + 5, (char *)(&i), 1);
  uint16_t j = __bswap_16(2 + (i << 2));
  memcpy(b + 2, (char *)(&j), 2);
  return b;
};

char *rewriteASPATH(char *aspathattribute, uint32_t as, uint8_t index) {
  assert((uint8_t)aspathattribute[5] >= index);
  assert((uint8_t)aspathattribute[1] == AS_PATH);
  static char b[4096] = {ExtendedLength, AS_PATH, 0, 2, AS_SEQUENCE, 0};
  uint32_t *to = (uint32_t *)(6 + aspathattribute + (index << 2));
  memcpy(to, &as, 4);
  return aspathattribute;
};
