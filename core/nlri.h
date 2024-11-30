#ifndef __NLRI_H
#define __NLRI_H

struct prefix {
  uint32_t ip;
  uint8_t length;
};
int cmp_prefix(struct prefix *pfxa, struct prefix *pfxb);
struct prefix *get_prefix_list(uint32_t ipstart, uint8_t length, int count, int seq);
struct prefix get_prefix_nlri(char *nlri);
struct bytestring nlris(uint32_t ipstart, uint8_t length, int count, int seq);
int nlri_member(struct bytestring nlris, struct prefix pfx);
char *showprefix(struct prefix pfx);
char *shownlris(struct bytestring nlris);
void *showprefixes(struct bytestring nlris);
int nlri_count(struct bytestring nlris);
int nlri_list(struct bytestring nlris, struct prefix **pfxs);

#endif
