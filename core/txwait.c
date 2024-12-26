#include <assert.h>
#include <linux/sockios.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <time.h>

#include "txwait.h"

// limit wait for cases where the peer disconnected (e.g. when sending to dead peer or fast disconnecting peer)
#define MAX_TXWAIT_COUNT 1000

void txwait(int sock) {
  int error, value;
  struct timespec delay = {0, 1000 * 100}; // 100 uS (units are nS)
  for (int i = 0; i < MAX_TXWAIT_COUNT; i++) {
    error = ioctl(sock, SIOCOUTQ, &value);
    if (0 == error && value > 0)
      nanosleep(&delay, NULL);
    else
      break;
  }
  if (0 != error)
    perror("txwait ioctl problem");
  else if (value > 0)
    fprintf(stderr, "txwait timed out wating for transmit queue to empty\n");
  assert(0 == error);
};
