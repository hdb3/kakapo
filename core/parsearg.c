
#include <arpa/inet.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kakapo.h"
#include "libutil.h"

// LIMIT: max number of sub-parameters in a single parameter string
#define LIMIT 4
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

  if (0 == *args.argv[0])
    p->remoteip = 0;
  else
    p->remoteip = toHostAddress(args.argv[0]);

  if ((args.argc < 2) || 0 == *args.argv[1]) // null string in pos 2 valid
    p->localip = 0;
  else
    p->localip = toHostAddress(args.argv[1]);

  if (args.argc < 3)
    p->as = 0;
  else {
    if (1 != sscanf(args.argv[2], "%d", &p->as)) {
      fprintf(stderr, "could not read an AS number\n");
      p->as = 0;
    }
  }

  if (args.argc < 4)
    p->port = 0;
  else {
    if (1 != sscanf(args.argv[3], "%hd", &p->port)) // %hd signals a 16 bit target
      die("could not read a TCP port\n");
  }
};

char *displaypeer(struct peer *p) {
  static char rval[100];
  int ix;
  ix = snprintf(rval, 100, "%s", fromHostAddress(p->remoteip));
  snprintf(rval + ix, 100 - ix, ":%d, %s , %d", p->port, fromHostAddress(p->localip), p->as);
  return rval;
};
