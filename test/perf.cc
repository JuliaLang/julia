// g++ -O2 perf.cc -o perf ../external/openblas-v0.1alpha2.2/libopenblas.a

#include <sys/time.h>
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include <cstring>
#include <complex>
#include <algorithm>

#include "../external/openblas-v0.1alpha2.2/cblas.h"

using namespace std;

#define NITER 10

double clock_now()
{
    struct timeval now;

    gettimeofday(&now, NULL);
    return (double)now.tv_sec + (double)now.tv_usec/1.0e6;
}

#define CLOCK() clock_now()

int fib(int n) {
    return n < 2 ? n : fib(n-1) + fib(n-2);
}

long parse_int(const char *s, long base) {
    long n = 0;
    
    for (int i=0; i<strlen(s); ++i) {
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

double *rand(int n) {
  double *d = (double *) malloc (n*sizeof(double));
  for (int k=0; k<n; ++k) d[k] = drand48();
  return d;
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
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
      double *a = ones(200,200);
      free(a);
    }
    t2 = CLOCK() - t1;
    printf("ones(200,200):\t %1.8lf\n", (t2/NITER));

    // A*A'
    //SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
    double *b = ones(200, 200);
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
      double *c = matmul_aat(200, b);
      free(c);
    }
    t2 = CLOCK() - t1;
    free(b);
    printf("A*A':   \t %1.8lf\n", (t2/NITER));

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
    t1 = CLOCK();
    for (int i=0; i<NITER; ++i) {
      double *d = rand(5000);
      sort(d, d+5000);
      free(d);
    }
    t2 = CLOCK() - t1;
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

    // randmatstat    
} 
