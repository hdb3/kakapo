
/* kakapo - a BGP traffic source and sink */

#ifndef VERBOSE
#define VERBOSE (0)
#endif

#define SOCKADDRSZ (sizeof (struct sockaddr_in))

extern int pid;
extern uint32_t SLEEP;
extern uint32_t TIMEOUT;
extern long long int idlethreshold;

extern uint32_t SEEDPREFIX;
extern uint32_t SEEDPREFIXLEN;
extern uint32_t GROUPSIZE;
extern uint32_t BLOCKSIZE;
extern uint32_t MAXBURSTCOUNT;
extern uint32_t NEXTHOP;
