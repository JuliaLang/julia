#include <stdio.h>

int xs[300] = {0,0,0,1,0};

int __attribute((noinline)) testUcharX(unsigned char x) {
    return xs[x];
}

#define xstr(s) str(s)
#define str(s) #s
volatile int (*fptr)(unsigned char x);
volatile int a;
volatile int b;

int main() {
    printf("all of the following should be 1 except xs[259] = 0");
    a = 3;
    b = 259;
    fptr = (volatile int (*)(unsigned char x))&testUcharX;
    if ((((long)fptr)&((long)1)<<32) == 1) fptr = NULL;
    printf("compiled with: '%s'\nxs[3] = %d\nxs[259] = %d\ntestUcharX(3) = %d\ntestUcharX(%d) = %d\nfptr(3) = %d\nfptr(259) = %d\n",
        xstr(CC), xs[a], xs[b], testUcharX(a), b, testUcharX(b), fptr(a), fptr(b));
}

