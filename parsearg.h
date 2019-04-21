
#include <stdint.h>
#define LIMIT 3

struct peer { uint32_t remote; uint32_t local; uint32_t as; };
struct peer parseargument (char *s);
char * displaypeer(struct peer p);
