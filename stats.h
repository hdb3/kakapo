
/*
#include <errno.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/sendfile.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/time.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <sys/ioctl.h>

#include "session.h"
#include "kakapo.h"
*/

#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include "libutil.h"
#define _1e6 (1000000L)

struct logrecord {
    inttime ts;
    uint64_t updates,nlri,withdrawn;
};

struct sessionlog {

    int tid;
    char * tids;
    int changed;
    pthread_mutex_t mutex;
    struct logrecord cumulative,current;
    struct sessionlog *next;

};

typedef struct sessionlog * slp_t;

void getsessionlog (slp_t slp, slp_t slog);
char * displaysessionlog (slp_t slp);
char * displaylogrecord (slp_t slp);
void updatelogrecord (slp_t slp, int nlri, int withdrawn);
slp_t initlogrecord (int tid, char* tids);
void startstatsrunner ();
