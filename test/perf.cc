// g++ -O2 perf.cc -o perf ../external/openblas-v0.1alpha2.4/libopenblas.a

#include <sys/time.h>
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include <cstring>
#include <complex>
#include <algorithm>

#include "../external/openblas-v0.1alpha2.4/cblas.h"

#define DSFMT_MEXP 19937
#include "../external/random/dsfmt-2.1/dSFMT.c"

using namespace std;

#define NITER 5

double clock_now()
{
    struct timeval now;

    gettimeofday(&now, NULL);
    return (double)now.tv_sec + (double)now.tv_usec/1.0e6;
}

int fib(int n) {
    return n < 2 ? n : fib(n-1) + fib(n-2);
}

long parse_int(const char *s, long base) {
    long n = 0;
    
    for (unsigned int i=0; i<strlen(s); ++i) {
        char c = s[i];
        long d = 0;
        if (c >= '0' && c <= '9') d = c-'0';
        else if (c >= 'A' && c <= 'Z') d = c-'A' + (int) 10;
        else if (c >= 'a' && c <= 'z') d = c-'a' + (int) 10;
        else exit(-1);

        if (base <= d) exit(-1);
        n = n*base + d;
    }
    return n;
}

double *ones(int m, int n) {
    double *a = (double *) malloc(m*n*sizeof(double));
    for (int k=0; k<m*n; ++k) {
        a[k] = 1.0;
    }
    return a;
}

double *matmul_aat(int n, double *b) {
    double *c = (double *) malloc(n*n*sizeof(double));
    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, n, n, n, 1.0, b, n, b, n, 0.0, c, n);
    return c;
}

int mandel(complex<double> z) {
    int n = 0;
    complex<double> c = complex<double>(real(z), imag(z));
    for (n=0; n<=79; ++n) {
        if (abs(z) > 2.0) {
            n -= 1;
            break;
        }
        z = pow(z,2)+c;
    }
    return n+1;
}

int mandelperf() {
    int mandel_sum = 0;
    for (double re=-2.0; re<=0.5; re+=0.1) {
        for (double im=-1.0; im<=1.0; im+=0.1) {
            int m = mandel(complex<double>(re, im));
            mandel_sum += m;
        }
    }
    return mandel_sum;
}

double *myrand(int n) {
    double *d = (double *) malloc (n*sizeof(double));
    for (int k=0; k<n; ++k) d[k] = dsfmt_gv_genrand_open_open();
    return d;
}

void quicksort(double *a, int lo, int hi) {
    int i = lo;
    int j = hi;
    while (i < hi) {
        double pivot = a[(lo+hi)/2];
        // Partition
        while (i <= j) {
            while (a[i] < pivot) {
                i = i + 1;
            }
            while (a[j] > pivot) {
                j = j - 1;
            }
            if (i <= j) {
                double t = a[i];
                a[i] = a[j];
                a[j] = t;
                i = i + 1;
                j = j - 1;
            }
        }

        // Recursion for quicksort
        if (lo < j) {
            quicksort(a, lo, j);
        }
        lo = i;
        j = hi;
    }
}

double pisum() {
    double sum = 0.0;
    for (int j=0; j<500; ++j) {
        sum = 0.0;
        for (int k=1; k<=10000; ++k) {
            sum += 1.0/(k*k);
        }
    }
    return sum;
}

void print_perf(const char *name, double t) {
    printf("c,%s,%.6f\n", name, t*1000);
}

int main() {
    // Initialize RNG
    dsfmt_gv_init_gen_rand(0);

    double t, tmin;
    
    // fib(20)
    assert(fib(20) == 6765);
    int f=0;
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        f += fib(20);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("fib", tmin);
    
    // parse_bin
    assert(parse_int("1111000011110000111100001111", 2) == 252645135);
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        for (int k=0; k<1000; ++k) {
            parse_int("1111000011110000111100001111", 2);
        }
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("parse_int", tmin);

    // array constructor
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        double *a = ones(200,200);
        free(a);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("ones", tmin);

    // A*A'
    //SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
    double *b = ones(200, 200);
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        double *c = matmul_aat(200, b);
        free(c);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    free(b);
    print_perf("AtA", tmin);

    // mandel
    int mandel_sum;
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        mandel_sum = mandelperf();
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    assert(mandel_sum == 14720);
    print_perf("mandel", tmin);

    // sort
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        double *d = myrand(5000);
        quicksort(d, 0, 5000-1);
        free(d);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("quicksort", tmin);

    // pi sum
    double pi;
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        pi = pisum();
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    assert(fabs(pi-1.644834071848065) < 1e-12);
    print_perf("pi_sum", tmin);

    return 0;
} 
