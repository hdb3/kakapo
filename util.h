

// util.h

int die(char *mess);
void printHex ( FILE * fd, unsigned char *buf, unsigned int l);
unsigned char *toHex (unsigned char *buf, unsigned int l);

long long int getinttime ();
long long int timeval_to_int (struct timeval *tval);

char* timeval_to_str (struct timeval *tval);

int timeval_subtract (struct timeval *result, struct timeval *x, struct timeval *y);
