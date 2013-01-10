#include <complex.h>

#define DSFMT_MEXP 19937
#include "perf.h"
#include "../../deps/random/randmtzig.c"

double *myrand(int n) {
    double *d = (double *)malloc(n*sizeof(double));
    dsfmt_gv_fill_array_close_open(d, n);
    return d;
}

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

int mandel(double complex z) {
    int n = 0;
    double complex c = z;
    for (n=0; n<=79; ++n) {
        if (cabs(z) > 2.0) {
            n -= 1;
            break;
        }
        z = cpow(z,2)+c;
    }
    return n+1;
}

int mandelperf() {
    int mandel_sum = 0;
    for (double re=-2.0; re<=0.5; re+=0.1) {
        for (double im=-1.0; im<=1.0; im+=0.1) {
            int m = mandel(re+im*I);
            mandel_sum += m;
        }
    }
    return mandel_sum;
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

struct double_pair { double s1, s2; };

struct double_pair randmatstat(int t) {
    int n = 5;
    struct double_pair r;
    double *v = (double*)calloc(t,sizeof(double));
    double *w = (double*)calloc(t,sizeof(double));
    double *a = (double*)malloc((n)*(n)*sizeof(double));
    double *b = (double*)malloc((n)*(n)*sizeof(double));
    double *c = (double*)malloc((n)*(n)*sizeof(double));
    double *d = (double*)malloc((n)*(n)*sizeof(double));
    double *P = (double*)malloc((n)*(4*n)*sizeof(double));
    double *Q = (double*)malloc((2*n)*(2*n)*sizeof(double));
    double *PtP1 = (double*)malloc((4*n)*(4*n)*sizeof(double));
    double *PtP2 = (double*)malloc((4*n)*(4*n)*sizeof(double));
    double *QtQ1 = (double*)malloc((2*n)*(2*n)*sizeof(double));
    double *QtQ2 = (double*)malloc((2*n)*(2*n)*sizeof(double));
    for (int i=0; i < t; i++) {
        randmtzig_fill_randn(a, n*n);
        randmtzig_fill_randn(b, n*n);
        randmtzig_fill_randn(c, n*n);
        randmtzig_fill_randn(d, n*n);
        memcpy(P+0*n*n, a, n*n*sizeof(double));
        memcpy(P+1*n*n, b, n*n*sizeof(double));
        memcpy(P+2*n*n, c, n*n*sizeof(double));
        memcpy(P+3*n*n, d, n*n*sizeof(double));
        for (int j=0; j < n; j++) {
            for (int k=0; k < n; k++) {
                Q[2*n*j+k]       = a[k];
                Q[2*n*j+n+k]     = b[k];
                Q[2*n*(n+j)+k]   = c[k];
                Q[2*n*(n+j)+n+k] = d[k];
            }
        }
        cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans,
                    n, n, 4*n, 1.0, P, 4*n, P, 4*n, 0.0, PtP1, 4*n);
        cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                    4*n, 4*n, 4*n, 1.0, PtP1, 4*n, PtP1, 4*n, 0.0, PtP2, 4*n);
        cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                    4*n, 4*n, 4*n, 1.0, PtP2, 4*n, PtP2, 4*n, 0.0, PtP1, 4*n);
        for (int j=0; j < n; j++) {
            v[i] += PtP1[(n+1)*j];
        }
        cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans,
                    2*n, 2*n, 2*n, 1.0, Q, 2*n, Q, 2*n, 0.0, QtQ1, 2*n);
        cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                    2*n, 2*n, 2*n, 1.0, QtQ1, 2*n, QtQ1, 2*n, 0.0, QtQ2, 2*n);
        cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                    2*n, 2*n, 2*n, 1.0, QtQ2, 2*n, QtQ2, 2*n, 0.0, QtQ1, 2*n);
        for (int j=0; j < 2*n; j++) {
            w[i] += QtQ1[(2*n+1)*j];
        }
    }
    free(PtP1);
    free(PtP2);
    free(QtQ1);
    free(QtQ2);
    free(P);
    free(Q);
    free(a);
    free(b);
    free(c);
    free(d);
    double v1=0.0, v2=0.0, w1=0.0, w2=0.0;
    for (int i=0; i < t; i++) {
        v1 += v[i]; v2 += v[i]*v[i];
        w1 += w[i]; w2 += w[i]*w[i];
    }
    free(v);
    free(w);
    r.s1 = sqrt((t*(t*v2-v1*v1))/((t-1)*v1*v1));
    r.s2 = sqrt((t*(t*w2-w1*w1))/((t-1)*w1*w1));
    return r;
}

double *randmatmul(int n) {
    double *A = myrand(n*n);
    double *B = myrand(n*n);
    double *C = (double*)malloc(n*n*sizeof(double));
    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                n, n, n, 1.0, A, n, B, n, 0.0, C, n);
    free(A);
    free(B);
    return C;
}

void printfd(int n) {
    FILE *f = fopen("/dev/null", "w");
    long i = 0;
    for (i = 0; i < n; i++)
        fprintf(f, "%ld %ld\n", i, i);
    fclose(f);
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
    int f = 0;
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        f += fib(20);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("fib", tmin);

    // parse_bin
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        char s[11];
        for (int k=0; k<1000; ++k) {
            uint32_t n = dsfmt_gv_genrand_uint32();
            sprintf(s, "%x", n);
            uint32_t m = (uint32_t)parse_int(s, 16);
            assert(m == n);
        }
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("parse_int", tmin);

    // // array constructor
    // tmin = INFINITY;
    // for (int i=0; i<NITER; ++i) {
    //     t = clock_now();
    //     double *a = ones(200,200);
    //     free(a);
    //     t = clock_now()-t;
    //     if (t < tmin) tmin = t;
    // }
    // print_perf("ones", tmin);
    // 
    // // A*A'
    // //SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
    // double *b = ones(200, 200);
    // tmin = INFINITY;
    // for (int i=0; i<NITER; ++i) {
    //     t = clock_now();
    //     double *c = matmul_aat(200, b);
    //     free(c);
    //     t = clock_now()-t;
    //     if (t < tmin) tmin = t;
    // }
    // free(b);
    // print_perf("AtA", tmin);

    // mandel
    int mandel_sum;
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        mandel_sum = mandelperf();
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    assert(mandel_sum == 14719);
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

    // rand mat stat
    struct double_pair r;
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        r = randmatstat(1000);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    // assert(0.5 < r.s1 && r.s1 < 1.0 && 0.5 < r.s2 && r.s2 < 1.0);
    print_perf("rand_mat_stat", tmin);

    // rand mat mul
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        double *C = randmatmul(1000);
        assert(0 <= C[0]);
        free(C);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("rand_mat_mul", tmin);

    // printfd
    tmin = INFINITY;
    for (int i=0; i<NITER; ++i) {
        t = clock_now();
        printfd(100000);
        t = clock_now()-t;
        if (t < tmin) tmin = t;
    }
    print_perf("printfd", tmin);

    return 0;
}
