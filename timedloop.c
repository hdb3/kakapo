#include <time.h>
#include <stdio.h>
#include "timespec.h"
#include "libutil.h"
#include "timedloop.h"

void timedloopms (int duration, int (action) (int)) {
    long int secs = duration / 1000L;
    long int nsecs = 1000000L * ( duration % 1000L);
    timedloop ( (struct timespec) { secs , nsecs}, action);
};

void timedloopsec (int duration, int (action) (int)) {
    timedloop ( (struct timespec) { duration , 0}, action);
};

void timedloop (struct timespec duration, int (action) (int)) {

   struct timespec delay, ts_target, ts_entry, ts_exit, now;
   //void gettime (struct timespec *ts) { int tmp = clock_gettime(CLOCK_REALTIME,ts); };
   int i = 0;
   int result;

   gettime(&ts_target);
   while (1) {
       gettime(&ts_entry);
       result = action(i);
       gettime(&ts_exit);
       //if ( result ) break;
       if ( -1 == result )
           break;
       else if ( 0 == result )
           ts_target = timespec_add (ts_target,duration);
       else
           ts_target = timespec_add (ts_target,(struct timespec) { result , 0 }); // action can request an additional delay, measured in seconds
       i++;
       gettime(&now);
       while (timespec_gt(ts_target,now)) {
           struct timespec delay = timespec_sub(ts_target,now);
           nanosleep(&delay,&now);
           gettime(&now);
       };
   };
};
