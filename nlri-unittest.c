#include <stdio.h>

#include "bytestring.h"
#include "nlri.h"

int main(int argc, char **argv) {
  printf("hello wrold\n");
  struct bytestring z = nlris(toHostAddress("10.0.0.0"), 30, 4, 0);
  showprefixes(z);
};
