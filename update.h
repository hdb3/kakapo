#include <stdio.h>
#include <stdint.h>
#include "bytestring.h"

struct bytestring pa2bytestring (char* pa);
//char * paOrigin;
extern char paOrigin [];
extern char paLocalPref [];
extern char paMED [];
char *paNextHop (uint32_t nexthop);
char *paASPATH(uint32_t *asn);
