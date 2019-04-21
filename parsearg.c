
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

// struct peer { uint32_t remote; uint32_t local; uint32_t as; };
struct peer parseargument (char *s) {
  uint32_t remote;
  uint32_t local = 0 ;
  uint32_t as = 0;
  args_t args = commaparse (s);
  remote = toHostAddress(args.argv[0]);
  if ( args.argc > 0 )
    local = toHostAddress(args.argv[1]);
  if ( args.argc > 1 )
    sscanf("%d", args.argv[1], &as);
  return (struct peer) { remote, local, as };
};
// Unit testing
// Cases: parse empty, valid and invalid subfields in each position, with 1,2,3 adn more parameters.  Build a display routein for struct peer
char * displaypeer(struct peer p) {
  static char rval[42];
  snprintf(rval,42,"(%s,%s,%d)",fromHostAddress(p.remote),fromHostAddress(p.local),p.as);
  return rval;
};
