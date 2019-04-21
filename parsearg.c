
#include <stdio.h>
#include <stdint.h>
#include <arpa/inet.h>
#include "util.h"
#include "parsearg.h"

#define LIMIT 3
args_t commaparse (char *s) {
  static char *argv [LIMIT];
  int  argc = 1;
  argv[0] = s;
  while ( (0 != *s) && (argc < LIMIT ) ) {
    if ( ',' == *s ) {
      *s = 0;
      argv[argc++] = s + 1;
    };
    s++;
  };
  return (args_t) { argc , argv };
};

struct peer parseargument (char *s) {
  uint32_t remote;
  uint32_t local;
  uint32_t as = 0;
  struct peer p;
  args_t args = commaparse (s);

  if (0 == *args.argv[0]) // null string in pos 1 valid, use 0.0.0.0, implies this is a listen only peer definition
    remote = 0;
  else
    remote = toHostAddress(args.argv[0]);

  if ( ( args.argc < 2 ) || 0 == *args.argv[1]) // null string in pos 2 valid, use 0.0.0.0, implies this is a default 0.0.0.0 listener
      local = 0;
    else
      local = toHostAddress(args.argv[1]);

  if ( ( args.argc < 3 ) || 0 == *args.argv[2]) // null string in pos 3 valid, use 0, implies this is a dynamic ASN
    as = 0;
  else
    sscanf(args.argv[2],"%d", &as);

  return (struct peer) { remote, local, as };
};
char * displaypeer(struct peer p) {
  static char rval[42];
  int ix;
  ix = snprintf(rval,42,"%s , ",fromHostAddress(p.remote));
       snprintf(rval+ix,42-ix,"%s , %d",fromHostAddress(p.local),p.as);

/*
  if ( 0 == p.remote )
    ix = snprintf(rval,42,",");
  else
    ix = snprintf(rval,42,"%s,",fromHostAddress(p.remote));

  if ( 0 == p.local )
    ix += snprintf(rval+ix,42-ix,",");
  else
    ix += snprintf(rval+ix,42-ix,"%s,",fromHostAddress(p.local));

  if (0 == p.as)
    snprintf(rval+ix,42-ix,",");
  else
    snprintf(rval+ix,42-ix,",%d",p.as);
*/
  return rval;
};
