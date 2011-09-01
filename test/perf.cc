// Compile on Mac with: g++ -O3 perf.cc -o perf  -framework Accelerate
// Need to compile cblas in openblas, so that we can link to that on both, linux and mac.

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <cassert>
#include <ctime>

#ifdef __APPLE__
#include <vecLib/cblas.h>
#endif

#define NITER 100

extern "C" void dgemm_(char, char, int, int, int, double, double *, int, double *, int, double, double *, int);

int fib(int n) {
    return n < 2 ? n : fib(n-1) + fib(n-2);
}

int main() {
    clock_t t1, t2;
    
    // fib(20)
    assert(fib(20) == 6765);
    int f=0;
    t1 = clock();
    for (int i=0; i<NITER; ++i) {
      f += fib(20);
    }
    t2 = clock() - t1;
    printf("fib(20):\t %1.8lf\n", (t2/(double)NITER) / (double) CLOCKS_PER_SEC);
    
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
    printf("ones(200,200):\t %1.8lf\n", (t2/NITER) / (double) CLOCKS_PER_SEC);

    // A*A'
    //SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
    #ifdef __APPLE__
    double *b, *c;
    b = (double *) malloc(200*200*sizeof(double));
    c = (double *) malloc(200*200*sizeof(double));
    for (int k=0; k<200*200; ++k) {
      a[k] = 1.0;
      c[k] = 0.0;
    }
    t1 = clock();
    for (int i=0; i<NITER; ++i) {
      cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, 200, 200, 200, 1.0, b, 200, b, 200, 0.0, c, 200);
    }
    t2 = clock() - t1;
    free(b);
    free(c);
    printf("A*A':   \t %1.8lf\n", (t2/NITER) / (double) CLOCKS_PER_SEC);
    #endif
} 
