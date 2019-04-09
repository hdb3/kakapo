
/* kakapo - a BGP traffic source and sink */

#ifndef VERBOSE
#define VERBOSE (0)
#endif

#define SOCKADDRSZ (sizeof (struct sockaddr_in))

extern int pid;
extern uint32_t SLEEP;
extern uint32_t TIMEOUT;
extern long long int idlethreshold;
