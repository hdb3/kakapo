#ifndef __NLRI_H
#define __NLRI_H

struct prefix {
  uint32_t ip;
  uint8_t length;
};

struct bytestring nlris(uint32_t ipstart, uint8_t length, int count, int seq);
int nlri_member(struct bytestring nlris, struct prefix pfx);
char *showprefix(struct prefix pfx);
char *shownlris(struct bytestring nlris);
void *showprefixes(struct bytestring nlris);

#endif
