#include "libutil.h"
#include <assert.h>
#include <errno.h>
#include <malloc.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

int sendbs(int sock, struct bytestring msg) {
  int res = send(sock, msg.data, msg.length, 0);
  if (res != msg.length) {
    fprintf(stderr, "send failure, res=%d, errno=%d\n", res, errno);
    return 0;
  } else
    return 1;
};

struct bytestring empty = {0, 0};

struct bytestring EOS = {0xffff, 0};

char *hexbytestring(struct bytestring bs) { return toHex(bs.data, bs.length); };

struct bytestring concatbytestring(struct bytestring bs0, ...) {

  if (bs0.length == 0xffff)
    return EOS;
  int length = bs0.length;
  // DEBUG // printf(" 0: %d/%d\n",length,length);
  va_list ap0;
  va_start(ap0, bs0);
  struct bytestring bs = va_arg(ap0, struct bytestring);
  int i = 1;
  while (0xffff != bs.length) {
    length += bs.length;
    // DEBUG // printf("%2d: %d/%d\n",i,bs.length,length);
    bs = va_arg(ap0, struct bytestring);
    i++;
  };
  va_end(ap0);
  // DEBUG // printf("%d strings, total length %d\n",i,length);

  va_list ap1;
  va_start(ap1, bs0);
  bs = bs0;
  int j = 0;
  char *buf = malloc(length);
  char *next = buf;
  do {
    if (bs.length != 0) {
      next = mempcpy(next, bs.data, bs.length);
    };
    j++;
    bs = va_arg(ap1, struct bytestring);
  } while (0xffff != bs.length);
  va_end(ap1);
  return (struct bytestring){length, buf};
};
