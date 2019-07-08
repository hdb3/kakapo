#include "txwait.h"
#include <linux/sockios.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <time.h>

void txwait(int sock) {
  int error, value;
  struct timespec delay = {0, 1000 * 100}; // 100 uS (units are nS)
  do {
    error = ioctl(sock, SIOCOUTQ, &value);
    if (0 == error && value > 0)
      nanosleep(&delay, NULL);
  } while (0 == error && value > 0);
  if (0 != error)
    perror("txwait ioctl problem");
};
