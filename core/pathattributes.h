#include "bytestring.h"
#include <stdint.h>
#include <stdio.h>

struct bytestring pas2bytestring(char *pa, ...);
struct bytestring pa2bytestring(char *pa);
extern char paOrigin[];
extern char paMED[];
char *paNextHop(uint32_t nexthop);
char *paLocalPref(uint32_t localpref);
char *paASPATH(uint32_t *asn);
char *rewriteASPATH(char *aspathattribute, uint32_t as, uint8_t index);
