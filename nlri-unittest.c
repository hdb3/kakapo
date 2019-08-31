#include <stdio.h>

#include "bytestring.h"
#include "nlri.h"

int main(int argc, char **argv) {
  struct prefix pfx;
  struct bytestring z;

  printf("nlri-unittest\n");

  z = nlris(toHostAddress("10.0.0.0"), 30, 4, 0);
  printf("nlri: %s\n", hexbytestring(z));
  showprefixes(z);

  pfx = (struct prefix){toHostAddress("10.0.0.0"), 30};
  printf("checking membership for %s: ", showprefix(pfx));
  printf("%s\n", nlri_member(z, pfx) ? "TRUE" : "FALSE");

  pfx = (struct prefix){toHostAddress("10.0.0.12"), 30};
  printf("checking membership for %s: ", showprefix(pfx));
  printf("%s\n", nlri_member(z, pfx) ? "TRUE" : "FALSE");

  pfx = (struct prefix){toHostAddress("11.0.0.0"), 30};
  printf("checking membership for %s: ", showprefix(pfx));
  printf("%s\n", nlri_member(z, pfx) ? "TRUE" : "FALSE");
};
