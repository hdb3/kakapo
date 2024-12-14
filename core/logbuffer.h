#ifndef __LOGBUFFER_H
#define __LOGBUFFER_H

#include "timespec.h"

struct log_record {
  struct timespec timestamp;
  uint64_t message_count;
};

struct logbuffer {
  int block_size;
  int buffer_size;
  int read_cursor;
  int write_cursor;
  int overrun_count;
  struct log_record *logrecords;
  struct timespec log_cycle_duration;
  struct timespec deadline;
  int received, sent;
  bool stop_flag;
};

void logbuffer_init(struct logbuffer *lb, int size, int bsize, struct timespec log_cycle_duration, struct timespec deadline);
void logbuffer_write(struct logbuffer *lb, struct log_record *lr);
struct log_record *logbuffer_read(struct logbuffer *lb);
void logbuffer_destroy(struct logbuffer *lb);
#endif
