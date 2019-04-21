
#include <stdio.h>
#include "parsearg.h"

// Unit testing
// Cases: parse empty, valid and invalid subfields in each position, with 1,2,3 adn more parameters.

void unittest (char * s) {
    printf ("%s ~ %s\n",s,displaypeer(parseargument(s)));
};
 
int main(int argc, char **argv) {
  int i;
  for (i=1 ; i < argc ; i++ ) {
    unittest (argv[i]);
    //printf ("%s ~ %s\n",argv[i],displaypeer(parseargument(argv[i])));
  };
};

