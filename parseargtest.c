
#include <stdio.h>
#include "parsearg.h"

int main(int argc, char **argv) {
  int i;
  for (i=0 ; i < argc ; i++ ) {
    printf ("%s ~ %s\n",argv[i],displaypeer(parseargument(argv[i])));
  };
};

