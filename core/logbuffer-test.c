#include <stdio.h>

#include "libutil.h"

int main(int argc, char **argv) {
  int i;
  struct logbuffer lb;
  struct log_record lr;
  struct log_record *lrp;
  logbuffer_init(&lb, 10, 100, (struct timespec){0, 0});

  for (i = 0; i < 4; i++) {
    lr.message_count = i;
    lr.timestamp = (struct timespec){42, 1000 * i};
    logbuffer_write(&lb, &lr);
  };

  lrp = logbuffer_read(&lb);
  lrp = logbuffer_read(&lb);

  for (i = 4; i < 12; i++) {
    lr.message_count = i;
    lr.timestamp = (struct timespec){42, 1000 * i};
    logbuffer_write(&lb, &lr);
  };

  lrp = logbuffer_read(&lb);
  while (NULL != lrp) {
    printf("index=%d ts=%f\n", lrp->message_count, timespec_to_double(lrp->timestamp));
    lrp = logbuffer_read(&lb);
  };

  printf("overrun_count=%d read_cursor=%d write_cursor=%d\n", lb.overrun_count, lb.read_cursor, lb.write_cursor);
  logbuffer_destroy(&lb);
};
