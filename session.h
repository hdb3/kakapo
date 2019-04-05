
/* kakapo-session - a BGP traffic source and sink */

struct sessiondata {
 int sock;
 int tidx;
 int localip;
 int peerip;
 int as;
 char * fn1;
 char * fn2;
};

//void session(int sock, char * fn1 , char * fn2);
//*void session(struct sessiondata *sd);
void *session(void *x);
