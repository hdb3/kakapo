#include <assert.h>
#include <linux/sockios.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <time.h>

#include "txwait.h"

extern bool txerror;

// limit wait for cases where the peer disconnected (e.g. when sending to dead peer or fast disconnecting peer)
#define MAX_TXWAIT_COUNT 200000

void txwait(int sock) {
  int error, value;
  struct timespec delay = {0, 1000 * 100}; // 100 uS (units are nS)
  for (int i = 0; i < MAX_TXWAIT_COUNT; i++) {
    if (txerror)
      break;
    error = ioctl(sock, SIOCOUTQ, &value);
    if (0 == error && value > 0)
      nanosleep(&delay, NULL);
    else
      break;
  }
  if (0 != error)
    perror("txwait ioctl problem");
  else if (value > 0) {
    fprintf(stderr, "txwait timed out waiting for transmit queue to empty\n");
    close(sock);
    txerror = true;
  }
  assert(0 == error);
};
