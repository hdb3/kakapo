#include "bytestring.h"

char * update_buffered(char *buf, struct bytestring nlri, struct bytestring withdrawn, struct bytestring pathattributes);
struct bytestring update(struct bytestring nlri, struct bytestring withdrawn, struct bytestring pathattributes);
struct bytestring iBGPpath(uint32_t nexthop, uint32_t localpref, uint32_t *asn);
struct bytestring eBGPpath(uint32_t nexthop, uint32_t med, uint32_t *asn);
