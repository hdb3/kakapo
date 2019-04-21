
#include "parsearg.h"
#include "util.h"
#include <arpa/inet.h>
#include <stdint.h>
#include <stdio.h>

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

struct peer parseargument(char *s) {
  struct peer p;
  args_t args = commaparse(s);

  if (0 == *args.argv[0]) // null string in pos 1 valid, use 0.0.0.0, implies
                          // this is a listen only peer definition
    p.remote = 0;
  else
    p.remote = toHostAddress(args.argv[0]);

  if ((args.argc < 2) ||
      0 == *args.argv[1]) // null string in pos 2 valid, use 0.0.0.0, implies
                          // this is a default 0.0.0.0 listener
    p.local = 0;
  else
    p.local = toHostAddress(args.argv[1]);

  if ((args.argc < 3) || 0 == *args.argv[2]) // null string in pos 3 valid, use
                                             // 0, implies this is a dynamic ASN
    p.as = 0;
  else
    if (1 != sscanf(args.argv[2], "%d", &p.as))
      die("could not read an AS number");

  return p;
};

char *displaypeer(struct peer p) {
  static char rval[42];
  int ix;
  ix = snprintf(rval, 42, "%s , ", fromHostAddress(p.remote));
  snprintf(rval + ix, 42 - ix, "%s , %d", fromHostAddress(p.local), p.as);
  return rval;
};
