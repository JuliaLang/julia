#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <cassert>
#include <ctime>

#define NITER 100

int fib(int n) {
    return n < 2 ? n : fib(n-1) + fib(n-2);
}

int main() {
    clock_t t1, t2;
    
    // fib(20)
    assert(fib(20) == 6765);
    t1 = clock();
    for (int i=0; i<NITER; ++i) {
        fib(20);
    }
    t2 = clock() - t1;
    printf("fib(20):\t %lf\n", (t2/NITER) / (double) CLOCKS_PER_SEC);
    
    // array constructor
    double *a;
    t1 = clock();
    for (int i=0; i<NITER; ++i) {
        a = (double *) malloc(200*200*sizeof(double));
        for (int k=0; k<200*200; ++k) {
            a[k] = 1.0;
        }
        free(a);
    }
    t2 = clock() - t1;
    printf("ones(200,200):\t %lf\n", (t2/NITER) / (double) CLOCKS_PER_SEC);
    
} 
