#include "bytestring.h"
#include <stdint.h>
#include <stdio.h>

struct bytestring pas2bytestring(char *pa, ...);
struct bytestring pa2bytestring(char *pa);
extern char paOrigin[];
char *paNextHop(uint32_t nexthop);
char *paMED(uint32_t med);
char *paLocalPref(uint32_t localpref);
char *paASPATH(uint32_t *asn);
char *rewriteASPATH(char *aspathattribute, uint32_t as, uint8_t index);
uint32_t *aspathbuild(uint32_t as0, ...);
