#include <stdlib.h>
#include <string.h>

#include "libutil.h"

void logbuffer_destroy(struct logbuffer *lb) {
  free(lb->logrecords);
  memset(lb, 0, sizeof(struct logbuffer));
};

void logbuffer_init(struct logbuffer *lb, int buffer_size, int block_size, struct timespec log_cycle_duration, struct timespec deadline) {
  lb->logrecords = calloc(sizeof(struct log_record), buffer_size);
  lb->read_cursor = 0;
  lb->write_cursor = 0;
  lb->overrun_count = 0;
  lb->block_size = block_size;
  lb->buffer_size = buffer_size;
  lb->log_cycle_duration = log_cycle_duration;
  lb->deadline = deadline;
  lb->received = 0;
  lb->sent = 0;
  lb->stop_flag = false;
};

void logbuffer_write(struct logbuffer *lb, struct log_record *lr) {
  int window = (lb->read_cursor - lb->write_cursor + lb->buffer_size) % lb->buffer_size;
  if (2 == window || 3 == window)
    lb->overrun_count++;
  else
    lb->logrecords[lb->write_cursor++] = *lr;
  lb->write_cursor %= lb->buffer_size;
};

struct log_record *logbuffer_read(struct logbuffer *lb) {
  lb->read_cursor %= lb->buffer_size;
  if (lb->read_cursor == lb->write_cursor)
    return NULL;
  else
    return (lb->logrecords) + lb->read_cursor++;
};
