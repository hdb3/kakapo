
#define _GNU_SOURCE
#include "util.h"
#include <arpa/inet.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kakapo.h"

args_t commaparse(char *s) {
  static char *argv[LIMIT];
  int argc = 1;
  argv[0] = s;
  while ((0 != *s) && (argc < LIMIT)) {
    if (',' == *s) {
      *s = 0;
      argv[argc++] = s + 1;
    };
    s++;
  };
  return (args_t){argc, argv};
};

void parseargument(struct peer *p, char *s) {
  args_t args = commaparse(strdupa(s));

  if (0 == *args.argv[0]) // null string in pos 1 valid, use 0.0.0.0, implies
                          // this is a listen only peer definition
    p->remoteip = 0;
  else
    p->remoteip = toHostAddress(args.argv[0]);

  if ((args.argc < 2) ||
      0 == *args.argv[1]) // null string in pos 2 valid, use 0.0.0.0, implies
                          // this is a default 0.0.0.0 listener
    p->localip = 0;
  else
    p->localip = toHostAddress(args.argv[1]);

  if ((args.argc < 3) || 0 == *args.argv[2]) // null string in pos 3 valid, use
                                             // 0, implies this is a dynamic ASN
    p->as = 0;
  else if (1 != sscanf(args.argv[2], "%d", &p->as))
    die("could not read an AS number");
};

char *displaypeer(struct peer *p) {
  static char rval[42];
  int ix;
  ix = snprintf(rval, 42, "%s , ", fromHostAddress(p->remoteip));
  snprintf(rval + ix, 42 - ix, "%s , %d", fromHostAddress(p->localip), p->as);
  return rval;
};
