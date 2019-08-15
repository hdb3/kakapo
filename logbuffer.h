#ifndef __LOGBUFFER_H
#define __LOGBUFFER_H

#include "timespec.h"

struct log_record {
  struct timespec ts;
  int index;
};

struct logbuffer {
  int block_size;
  int buffer_size;
  int read_cursor;
  int write_cursor;
  int overrun_count;
  struct log_record *logrecords;
  struct timespec duration;
};

void logbuffer_init(struct logbuffer *lb, int size, int bsize, struct timespec duration);
void logbuffer_write(struct logbuffer *lb, struct log_record *lr);
struct log_record *logbuffer_read(struct logbuffer *lb);
void logbuffer_destroy(struct logbuffer *lb);
#endif
