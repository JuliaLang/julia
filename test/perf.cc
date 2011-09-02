// Compile on Mac with: g++ -O2 perf.cc -o perf  -framework Accelerate
// Need to compile cblas in openblas, so that we can link to that on both, linux and mac.

#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include <complex>
#include <sys/time.h>
#include <algorithm>

#ifdef __APPLE__
#include <vecLib/cblas.h>
#endif

using namespace std;

#define NITER 10

double clock_now()
{
    struct timeval now;

    gettimeofday(&now, NULL);
    return (double)now.tv_sec + (double)now.tv_usec/1.0e6;
}

#define CLOCK() clock_now()

extern "C" void dgemm_(char, char, int, int, int, double, double *, int, double *, int, double, double *, int);

int fib(int n) {
    return n < 2 ? n : fib(n-1) + fib(n-2);
}

// pi sum
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

long parse_int(const char *s, long base) {
    long n = 0;
    
    for (int i=0; i<strlen(s); ++i) {
        char c = s[i];
        long d = (long) '0' <= c <= '9' ? c-'0' :
                        'A' <= c <= 'Z' ? c-'A' + (int) 10 :
                        'a' <= c <= 'z' ? c-'a' + (int) 10 :
                        printf("Error\n");
        if (base <= d) {
            printf("Error\n");
        }
        n = n*base + d;
    }
    return n;
}

int main() {
    double t1, t2;
    
    // fib(20)
    assert(fib(20) == 6765);
    int f=0;
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
      f += fib(20);
    }
    t2 = CLOCK() - t1;
    printf("fib(20):\t %1.8lf\n", (t2/(double)NITER));
    
    // parse_bin
    assert(parse_int("1111000011110000111100001111", 2) == 252645135);
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
        for (int k=0; k<1000; ++k) {
            long n = parse_int("1111000011110000111100001111", 2);
        }
    }
    t2 = CLOCK() - t1;
    printf("parse_bin:\t %1.8lf\n", (t2/(double)NITER));

    // array constructor
    double *a;
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
        a = (double *) malloc(200*200*sizeof(double));
        for (int k=0; k<200*200; ++k) {
            a[k] = 1.0;
        }
        free(a);
    }
    t2 = CLOCK() - t1;
    printf("ones(200,200):\t %1.8lf\n", (t2/NITER));

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
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
      cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, 200, 200, 200, 1.0, b, 200, b, 200, 0.0, c, 200);
    }
    t2 = CLOCK() - t1;
    free(b);
    free(c);
    printf("A*A':   \t %1.8lf\n", (t2/NITER));
    #endif

    // mandel
    int mandel_sum;
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
        mandel_sum = mandelperf();
    }
    assert(mandel_sum == 14720);
    t2 = CLOCK() - t1;
    printf("mandel:   \t %1.8lf\n", (t2/NITER));

    // sort
    double *d;
    d = (double *) malloc(5000*sizeof(double));
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
      for (int k=0; k<5000; ++k) d[k] = drand48();
      sort(d, d+5000);
    }
    t2 = CLOCK() - t1;
    free(d);
    printf("sort:   \t %1.8lf\n", (t2/NITER));

    // pi sum
    t1 = CLOCK();
    double pi;
    for (int i=0; i<NITER; ++i) {
      pi = pisum();
    }
    assert(fabs(pi-1.644834071848065) < 1e-12);
    t2 = CLOCK() - t1;
    printf("pisum:   \t %1.8lf\n", (t2/NITER));
} 
