
// util.c

#include "util.h"

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <netinet/in.h>

// Parsing address parameters
// The subject can be either 192.168.1.2,8.8.8.8 or 192.168.1.2.  If it is the shorter form then the second value is read as if 0.0.0.0.

int parseAddress (char* s, struct in_addr* dst , struct in_addr* src) {
   char* commaloc = strchr(s,',');
   if (NULL == commaloc) {
        // default second value to zero / 0.0.0.0
        *src = (struct in_addr){0};
        return inet_aton(s, dst);
    } else {
        *commaloc = 0;
        return ( inet_aton(s, dst) | inet_aton(commaloc+1, src));
    }
}

int die(char *mess) {
  if (0 != errno)
    perror(mess);
  else
    fprintf(stderr, "%s\n", mess);
  exit(1);
}
