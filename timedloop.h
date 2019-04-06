#include <time.h>

void timedloop (struct timespec duration, int (action) (int));
void timedloopms (int duration, int (action) (int));
void timedloopsec (int duration, int (action) (int));
