
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>
#include "util.h"
#include "parsearg.h"

#define LIMIT 3
struct args { int argc; char **argv; };
typedef struct args args_t;
args_t commaparse (char *s) {
  static char *argv [LIMIT];
  int  argc = 0;
  argv[0] = s;
  while ( (0 != *s) && (argc < LIMIT ) ) {
    if ( ',' == *s ) {
      *s = 0;
      argv[++argc] = s + 1;
    };
    s++;
  };
  return (args_t) { argc , argv };
};

struct peer parseargument (char *s) {
  uint32_t remote;
  uint32_t local = 0 ;
  uint32_t as = 0;
  args_t args = commaparse (s);
  remote = toHostAddress(args.argv[0]);
  if ( args.argc > 0 )
    local = toHostAddress(args.argv[1]);
  if ( args.argc > 1 )
    sscanf(args.argv[2],"%d", &as);
  return (struct peer) { remote, local, as };
};
char * displaypeer(struct peer p) {
  static char rval[42];
  int ix = snprintf(rval,42,"(%s,",fromHostAddress(p.remote));
           snprintf(rval+ix,42-ix,"%s,%d)",fromHostAddress(p.local),p.as);
  return rval;
};
