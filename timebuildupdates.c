/* kakapo-session - a BGP traffic source and sink */

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/sendfile.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <unistd.h>

//#include "kakapo.h"
#include "libutil.h"
#include "stats.h"

#define BUFFSIZE 0x10000

#define BGPENDOFSTREAM (-1)
#define BGPTIMEOUT 0
#define BGPOPEN 1
#define BGPUPDATE 2
#define BGPNOTIFICATION 3
#define BGPKEEPALIVE 4
#define BGPUNKNOWN 5

#define NOTIFICATION_CEASE 6
#define NOTIFICATION_ADMIN_RESET 4

uint32_t localip, SEEDPREFIX;
uint32_t SEEDPREFIXLEN = 30;
int BLOCKSIZE;
uint32_t GROUPSIZE;
struct timespec tstart, tend, tdelta;

struct bytestring build_update_block(int bsn, int cyclenumber) {

  int i, usn;
  struct bytestring *vec = malloc(sizeof(struct bytestring) * BLOCKSIZE);
  int buflen = 0;

  // the loopcount runs from 0 to BLOCKSIZE -1, + a fixed offset (bsn * BLOCKSIZE)
  // an array to hold the entire output is exactly BLOCKSIZE in size
  // for (usn = bsn * BLOCKSIZE; usn < (bsn + 1) * BLOCKSIZE; usn++) {
  for (i = 0; i < BLOCKSIZE; i++) {
    usn = i + bsn * BLOCKSIZE;
    struct bytestring b = update(nlris(SEEDPREFIX, SEEDPREFIXLEN, GROUPSIZE, usn), empty, iBGPpath(localip, (uint32_t[]){usn + SEEDPREFIX, cyclenumber + 1, 0}));
    vec[i] = b;
    buflen += b.length;
  };
  char *data = malloc(buflen);
  char *offset = data;

  for (i = 0; i < BLOCKSIZE; i++) {
    offset = mempcpy(offset, vec[i].data, vec[i].length);
    free(vec[i].data);
  };
  return (struct bytestring){buflen, data};
};

int main(int argc, char **argv) {
  localip = toHostAddress("192.168.0.1");
  SEEDPREFIX = toHostAddress("10.0.0.0");
  BLOCKSIZE = atol(argv[1]);
  GROUPSIZE = atol(argv[2]);
  printf("generating table size %d / %d (%d)\n", BLOCKSIZE, GROUPSIZE, BLOCKSIZE * GROUPSIZE);
  gettime(&tstart);
  struct bytestring bs = build_update_block(0, 0);
  gettime(&tend);
  tdelta = timespec_sub(tend, tstart);
  printf("complete in %ld\n", timespec_to_ms(tdelta));
};
