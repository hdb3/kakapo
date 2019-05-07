
#include "util.h"
#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
  int i;
  struct in_addr src, dst;
  printf("parse test\n");
  for (i = 1; i < argc; i++) {
    int res = parseAddress(argv[i], &src, &dst);
    if (0 == res) {
      printf("failed to parse arg %d: %s\n", i, argv[i]);
    } else {
      printf("parsed arg %d OK: %s ", i, inet_ntoa(src));
      printf("%s\n", inet_ntoa(dst));
    };
  };
};
