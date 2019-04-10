
/* kakapo-session - a BGP traffic source and sink */

/*
#include <errno.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/sendfile.h>
#include <fcntl.h>
#include <sys/time.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <sys/ioctl.h>

#include "session.h"
*/

#include "stats.h"

#include <stdio.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>
#include "libutil.h"
#include "kakapo.h"
#define _1e6 (1000000L)
static slp_t statsbase = NULL;

void closelogrecord (slp_t slp, int tid) {
    assert (tid == slp->tid);
    // rather than free resources we simply mark the record closed
    slp->closed = 1;
};

slp_t initlogrecord (int tid, char* tids) {
    slp_t slp = malloc(sizeof(struct sessionlog));
    inttime now = getinttime();
    pthread_mutex_init(&slp->mutex,NULL);
    slp->tid = tid;
    slp->tids = strdup(tids);
    slp->changed = 1;
    slp->closed = 0;
    slp->firstts = TSZERO;
    slp->lastts = TSZERO;
    slp->cumulative.ts = now;
    slp->cumulative.updates=0;
    slp->cumulative.nlri=0;
    slp->cumulative.withdrawn=0;
    slp->current.ts = now;
    slp->current.updates=0;
    slp->current.nlri=0;
    slp->current.withdrawn=0;
    slp->next = statsbase;
    statsbase = slp;
    return slp;
};

void idlecheck (slp_t slp, struct timespec * now) {
    if (
          ( 0 != slp->firstts.tv_sec) &&
          ( timespec_gt ( (struct timespec) {idlethreshold,0} ,
                          timespec_sub (*now,
                                        slp->lastts
                                       )
                        )
          )
       )
    {
          struct timespec duration = timespec_sub (slp->lastts , slp->firstts);
          slp->lastburstduration = duration;
          fprintf(stderr,"%s burst duration %f\n", slp->tids, timespec_to_double(duration));
          slp->firstts = TSZERO;
          slp->lastts = TSZERO;
    };
};

void updatelogrecord (slp_t slp, int nlri, int withdrawn, struct timespec * ts) {
    pthread_mutex_lock(&slp->mutex);
    if (timespec_eq(TSZERO, slp->firstts))
        slp->firstts = *ts;
    slp->lastts = *ts;
    slp->current.updates++;
    slp->current.nlri += nlri;
    slp->current.withdrawn += withdrawn;
    slp->changed = 1;
    pthread_mutex_unlock(&slp->mutex);
};

char * displaylogrecord (slp_t slp) {
    char *s;
    inttime now = getinttime();
    pthread_mutex_lock(&slp->mutex);
    int tmp = asprintf(&s,"elapsed time : %f update msg cnt  %6ld (%6ld) NLRI cnt  %6ld (%6ld) withdrawn cnt  %6ld (%6ld), last burst %f" ,
      (now - slp->cumulative.ts ) / 1e6,
      slp->current.updates ,
      slp->cumulative.updates ,
      slp->current.nlri ,
      slp->cumulative.nlri ,
      slp->current.withdrawn ,
      slp->cumulative.withdrawn,
      timespec_to_double(slp->lastburstduration) );
    pthread_mutex_unlock(&slp->mutex);
    return s;
};

char * displaysessionlog (slp_t slp) {
    char *s;
    char bm [256] = "idle";
    pthread_mutex_lock(&slp->mutex);
    ///if (0 == slp->firstts)
        ///bm = strdup("idle");
    ///else
    if (0 != slp->firstts.tv_sec)
        snprintf(bm,255,"current burst %f", timespec_to_double(timespec_sub(slp->lastts , slp->firstts)));
    int tmp = asprintf(&s,"elapsed time : %f update msg rate %6ld (%6ld) NLRI rate %6ld (%6ld) withdrawn rate %6ld (%6ld) %s" ,
      slp->cumulative.ts / 1e6,
      slp->current.updates ,
      slp->cumulative.updates ,
      slp->current.nlri ,
      slp->cumulative.nlri ,
      slp->current.withdrawn ,
      slp->cumulative.withdrawn,
      bm);
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

    slp->changed = 0;
    pthread_mutex_unlock(&slp->mutex);
};

static void statsreport () {
    struct sessionlog tmp;
    struct timespec now;
    gettime(&now);
    slp_t slp=statsbase;
    int active=0;
    while (slp) {
        if (0==slp->closed) {
            active++;
            idlecheck(slp,&now);
            fprintf(stderr, "%s: counters: %s\e[K\n",slp->tids,displaylogrecord (slp));
            getsessionlog(slp,&tmp);
            fprintf(stderr, "%s: rate:     %s\e[K\n",slp->tids,displaysessionlog (&tmp));
        };
        slp=slp->next;
    };
    if (active)
        fprintf(stderr, "\e[%dA",active*2);
};

void startstatsrunner () {

    void * statsrunner (void * arg) {
        while (1) {
            statsreport();
            sleep(1);
        };
    };

  pthread_t thrd;
  pthread_create(&thrd, NULL, statsrunner, NULL);
};
