#include "timedloop.h"
#include "timespec.h"
#include <stdio.h>
#include <time.h>

int f(int p) {
  printf("f(%d)\n", p);
  if (p == 1) {
    printf("delaying for 4 seconds\n");
    return 4;
  } else if (p > 4)
    return (-1);
  else
    return 0;
};

int main(int argc, char **argv) {
  timedloop((struct timespec){0, 0}, f);
  timedloopms(50, f);
  timedloopms(500, f);
  timedloop((struct timespec){1, 0}, f);
};
