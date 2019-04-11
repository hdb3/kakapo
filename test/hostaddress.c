#include "util.h"
#include <stdio.h>

int main(int argc, char **argv) {

  printf("hostaddress test suite\n");

  char *ip = "192.168.0.1";

  printf("toHostAddress %s = %08x\n", ip, toHostAddress(ip));
  printf("fromHostAddress ( toHostAddress %s ) = %s\n", ip,
         fromHostAddress(toHostAddress(ip)));
};
