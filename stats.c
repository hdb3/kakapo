
/* kakapo-session - a BGP traffic source and sink */

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

#include "stats.h"

#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include "libutil.h"
#define _1e6 (1000000L)

slp_t initlogrecord () {
    slp_t slp = malloc(sizeof(struct sessionlog));
    inttime now = getinttime();
    pthread_mutex_init(&slp->mutex,NULL);
    slp->cumulative.ts = now;
    slp->cumulative.updates=0;
    slp->cumulative.nlri=0;
    slp->cumulative.withdrawn=0;
    slp->current.ts = now;
    slp->current.updates=0;
    slp->current.nlri=0;
    slp->current.withdrawn=0;
    return slp;
};

void updatelogrecord (slp_t slp, int nlri, int withdrawn) {
    pthread_mutex_lock(&slp->mutex);
    slp->current.updates++;
    slp->current.nlri += nlri;
    slp->current.withdrawn += withdrawn;
    pthread_mutex_unlock(&slp->mutex);
};

char * displaylogrecord (slp_t slp) {
    char *s;
    inttime now = getinttime();
    pthread_mutex_lock(&slp->mutex);
    int tmp = asprintf(&s,"elapsed time : %f (%f) update msg cnt %ld (%ld) NLRI cnt %ld (%ld) withdrawn cnt %ld (%ld)" ,
      (now - slp->current.ts ) / 1e6,
      (now - slp->cumulative.ts ) / 1e6,
      slp->current.updates ,
      slp->cumulative.updates ,
      slp->current.nlri ,
      slp->cumulative.nlri ,
      slp->current.withdrawn ,
      slp->cumulative.withdrawn);
    pthread_mutex_unlock(&slp->mutex);
    return s;
};

char * displaysessionlog (slp_t slp) {
    char *s;
    pthread_mutex_lock(&slp->mutex);
    int tmp = asprintf(&s,"elapsed time : %f (%f) update msg rate %ld (%ld) NLRI rate %ld (%ld) withdrawn rate %ld (%ld)" ,
      slp->current.ts / 1e6,
      slp->cumulative.ts / 1e6,
      slp->current.updates ,
      slp->cumulative.updates ,
      slp->current.nlri ,
      slp->cumulative.nlri ,
      slp->current.withdrawn ,
      slp->cumulative.withdrawn);
    pthread_mutex_unlock(&slp->mutex);
    return s;
};

void getsessionlog (slp_t slp, slp_t slog) {

// ends the current interval as well as reporting on it

    pthread_mutex_lock(&slp->mutex);

    inttime now = getinttime();
    inttime deltaCumulative = now - slp->cumulative.ts;
    inttime deltaCurrent = now - slp->current.ts;
    slog->cumulative.ts = deltaCumulative;
    slog->current.ts = deltaCurrent;

// update cumulative counters from current
     slp->cumulative.updates += slp->current.updates;
     slp->cumulative.nlri += slp->current.nlri;
     slp->cumulative.withdrawn += slp->current.withdrawn;

// calculate current rates
    slog->current.updates =  slp->current.updates * _1e6 / deltaCurrent; //all integer arithmetic!
    slog->current.nlri = slp->current.nlri * _1e6 / deltaCurrent; //all integer arithmetic!
    slog->current.withdrawn = slp->current.withdrawn * _1e6 / deltaCurrent; //all integer arithmetic!

// calculate cumulative rates
    slog->cumulative.updates = slp->cumulative.updates * _1e6 / deltaCumulative; //all integer arithmetic!
    slog->cumulative.nlri = slp->cumulative.nlri * _1e6 / deltaCumulative; //all integer arithmetic!
    slog->cumulative.withdrawn = slp->cumulative.withdrawn * _1e6 / deltaCumulative; //all integer arithmetic!

// reset the current counters
     slp->current.ts = now;
     slp->current.updates=0;
     slp->current.nlri=0;
     slp->current.withdrawn=0;

    pthread_mutex_unlock(&slp->mutex);
};
