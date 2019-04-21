
#include <stdio.h>
#include "parsearg.h"

// Unit testing
// Cases: parse empty, valid and invalid subfields in each position, with 1,2,3 adn more parameters.

void unittest (char * s) {
    printf ("%s ~",s);
    fflush(stdout);
    printf ("~ %s\n",displaypeer(parseargument(s)));
    //printf ("%s ~ %s\n",s,displaypeer(parseargument(s)));
};
 
int main(int argc, char **argv) {
  int i;
  for (i=1 ; i < argc ; i++ ) {
    unittest (argv[i]);
    //printf ("%s ~ %s\n",argv[i],displaypeer(parseargument(argv[i])));
  };

  unittest("192.168.1.1");

  unittest(",");
  unittest(",192.168.1.2");
  unittest("192.168.1.1,");
  unittest("192.168.1.1,192.168.1.2");

  unittest(",,");
  unittest("192.168.1.1,,");
  unittest(",192.168.1.2,");
  unittest(",,12345");
  unittest("192.168.1.1,192.168.1.2,");
  unittest("192.168.1.1,,12345");
  unittest(",192.168.1.2,12345");
};

