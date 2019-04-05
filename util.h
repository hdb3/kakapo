

// util.h

#include <sys/time.h>
int die(char *mess);
int fromHex (char* s);
void printHex ( FILE * fd, unsigned char *buf, unsigned int l);
unsigned char *toHex (unsigned char *buf, unsigned int l);

long long int getinttime ();
long long int timeval_to_int (struct timeval *tval);

char* timeval_to_str (struct timeval *tval);

int timeval_subtract (struct timeval *result, struct timeval *x, struct timeval *y);
