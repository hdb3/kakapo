#include "libutil.h"
#include <assert.h>
#include <errno.h>
#include <malloc.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

struct bytestring empty = {0, 0};

struct bytestring EOS = {0xffffffff, 0};

char *hexbytestring(struct bytestring bs) { return toHex(bs.data, bs.length); };

struct bytestring concatbytestring(struct bytestring bs0, ...) {

  if (bs0.length == 0xffffffff)
    return EOS;
  uint64_t length = bs0.length;
  va_list ap0;
  va_start(ap0, bs0);
  struct bytestring bs = va_arg(ap0, struct bytestring);
  int i = 1;
  while (0xffffffff != bs.length) {
    length += bs.length;
    bs = va_arg(ap0, struct bytestring);
    i++;
  };
  va_end(ap0);

  va_list ap1;
  va_start(ap1, bs0);
  bs = bs0;
  uint64_t j = 0;
  char *buf = malloc(length);
  char *next = buf;
  do {
    if (bs.length != 0) {
      next = mempcpy(next, bs.data, bs.length);
    };
    j++;
    bs = va_arg(ap1, struct bytestring);
  } while (0xffffffff != bs.length);
  va_end(ap1);
  return (struct bytestring){length, buf};
};
