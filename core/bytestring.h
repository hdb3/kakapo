#include <malloc.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "util.h"

#ifndef _BYTESTRING_H
#define _BYTESTRING_H

struct bytestring {
  uint64_t length;
  char *data;
};
extern struct bytestring empty;
extern struct bytestring EOS;
char *hexbytestring(struct bytestring bs);
struct bytestring concatbytestring(struct bytestring bs0, ...);
// int sendbs(int sock, struct bytestring msg);
#endif // _BYTESTRING_H
