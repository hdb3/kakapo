
#include <stdio.h>
#include <stdarg.h>

char *sconcat (char *s0,...) {
    printf(" 0: %s\n",s0);
    va_list ap;
    va_start(ap,s0);
    char *s = va_arg(ap,char*);
    int i = 1;
    while (0 != s) {
        printf("%2d: %s\n",i,s);
        s = va_arg(ap,char*);
        i++;
    };
};

int main(int argc, char **argv) {
    switch (argc) {
        case 0 : break;
        case 1 : printf("%d %s\n",argc,sconcat(argv[1],0)); break;
        case 2 : printf("%d %s\n",argc,sconcat(argv[1],argv[2],0)); break;
        case 3 : printf("%d %s\n",argc,sconcat(argv[1],argv[2],argv[3],0)); break;
        case 4 : printf("%d %s\n",argc,sconcat(argv[1],argv[2],argv[3],argv[4],0)); break;
        case 5 : printf("%d %s\n",argc,sconcat(argv[1],argv[2],argv[3],argv[4],argv[5],0)); break;
    };
};
