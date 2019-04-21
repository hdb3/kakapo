
#include <stdio.h>
#include <string.h>
#include "parsearg.h"

// Unit testing
// Cases: parse empty, valid and invalid subfields in each position, with 1,2,3 adn more parameters.

// commaparse unittest
void _unittest (char * s) {
    char locs[255];
    strncpy(locs,s,254);
    locs[254]=0;

    printf("commaparse(%s)\n",locs);
    args_t args = commaparse(locs);
    printf("returned %d values\n",args.argc);
    int c;
    for (c=0; c < args.argc ; c++) {
      printf ("%d: %s\n",c,args.argv[c]);
    };
};

// parseargument unittest
void unittest (char * s) {
    char locs[255];
    strncpy(locs,s,254);
    locs[254]=0;

    printf ("\'%s\' ~",locs);
    fflush(stdout);
    printf ("~ %s\n",displaypeer(parseargument(locs)));
    ////printf ("%s ~ %s\n",s,displaypeer(parseargument(locs)));
};
 
int main(int argc, char **argv) {
  if ( 1 < argc) {
    int i;
    for (i=1 ; i < argc ; i++ ) {
      unittest (argv[i]);
      //printf ("%s ~ %s\n",argv[i],displaypeer(parseargument(argv[i])));
    };
  } else {

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
};

