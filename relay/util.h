// util.h

#ifndef __UTIL_H
#define __UTIL_H
#include <netinet/in.h>

int parseAddress(char *s, struct in_addr *dst, struct in_addr *src);
int die(char *mess);

#endif
