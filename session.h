
/* kakapo-session - a BGP traffic source and sink */
// sessiondata is the structure passed to new threads from main

#define ROLELISTENER 1
#define ROLESENDER   2
struct sessiondata {
  int sock;
  int tidx;
  int as;
  int role;
};

//*void session(struct sessiondata *sd);
void *session(void *x);
