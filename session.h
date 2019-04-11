
/* kakapo-session - a BGP traffic source and sink */
// sessiondata is the structure passed to new threads from main

struct sessiondata {
  int sock;
  int tidx;
  int as;
};

//*void session(struct sessiondata *sd);
void *session(void *x);
