#include <stdio.h>

#include "libutil.h"

int main(int argc, char *argv[]) {
  fprintf(stderr, "hello from unit.c\n");

  fprintf(stderr, "test suite for variable precision time display functions from util.c\n");

  struct timespec ts;
  gettime(&ts);

  fprintf(stderr, "the time from showtime() is        %s\n", showtime(&ts));

  fprintf(stderr, "the time from showtime_prec(9) is  %s\n", showtime_prec(&ts, 9));
  fprintf(stderr, "the time from showtime_prec(6) is  %s\n", showtime_prec(&ts, 6));
  fprintf(stderr, "the time from showtime_prec(3) is  %s\n", showtime_prec(&ts, 3));

  fprintf(stderr, "the time from showtime_prec(10) is %s\n", showtime_prec(&ts, 10));
  fprintf(stderr, "the time from showtime_prec(0) is  %s\n", showtime_prec(&ts, 0));
}
