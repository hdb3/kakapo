
#ifndef __STATS_H
#define __STATS_H
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>

#include "libutil.h"

#define _1e6 (1000000L)

struct logrecord {
  inttime ts;
  uint64_t updates, nlri, withdrawn;
};

struct sessionlog {

  int tid;
  char *tids;
  int changed;
  int closed;
  int rcvseq;
  pthread_mutex_t mutex;
  struct logrecord cumulative, current;
  struct sessionlog *next;
  struct timespec firstts, lastts, lastburstduration;
};

typedef struct sessionlog *slp_t;

void getsessionlog(slp_t slp, slp_t slog);
char *displaysessionlog(slp_t slp);
char *displaylogrecord(slp_t slp);
void updatelogrecord(slp_t slp, int nlri, int withdrawn, struct timespec *ts);
slp_t initlogrecord(int tid, char *tids);
void closelogrecord(slp_t slp, int tid);
void startstatsrunner();
#endif
