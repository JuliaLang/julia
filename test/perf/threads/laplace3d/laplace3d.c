// This file is a part of Julia. License is MIT: http://julialang.org/license

// GCC command line: gcc -fopenmp -mavx2 laplace3d.c -o laplace3d

/* Laplace 3D

   orig:        simple serial version
   naive:       simple parallelized version
   auto:        some ninja knowledge, using icc directives
   sse/avx:     ninja-optimized

   Requires Sandy Bridge and up.

   Note that the SSE/AVX versions do not handle boundary conditions
   and thus each dimension must be 4n+2/8n+2. Try 258x258x258.

   2014.08.06   anand.deshpande         Initial code.
   2014.08.06   dhiraj.kalamkar         Padding and streaming stores.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include <unistd.h>
#include <immintrin.h>
#include <omp.h>

#if defined(__i386__)
static inline uint64_t rdtsc(void)
{
    uint64_t x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
}
#elif defined(__x86_64__)
static inline uint64_t rdtsc(void)
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ((uint64_t)lo) | (((uint64_t)hi) << 32);
}
#elif defined(_COMPILER_MICROSOFT_)
#include <intrin.h>
static inline uint64_t rdtsc(void)
{
    return __rdtsc();
}
#endif

void l3d_naive(int nx, int padded_nx, int ny, int nz, float *u1, float *u2);
void l3d_auto(int nx, int padded_nx, int ny, int nz, float *u1, float *u2);
void l3d_sse(int nx, int padded_nx, int ny, int nz, float *u1, float *u2);
void l3d_avx(int nx, int padded_nx, int ny, int nz, float *u1, float *u2);
void l3d_orig(int nx, int ny, int nz, float *u1, float *u2);

double cpughz()
{
    uint64_t t0 = rdtsc();
    sleep(1);
    uint64_t onesec = rdtsc() - t0;
    return onesec*1.0/1e9;
}

int main(int argc, char **argv)
{
    int nx, padded_nx, ny, nz, iters, i, j, k, ind, p_ind, verify,
        nthreads, pad_size;
    float *u1, *u1_p, *u1_orig, *u2, *u2_p, *u2_orig, *foo,
        error_tol = 0.00001;
    double ghz;
    void (*l3d)(int nx, int padded_nx, int ny, int nz,
        float *u1, float *u2);

    if (argc != 7) {
        fprintf(stderr, "Usage:\n"
            "  laplace3d <nx> <ny> <nz> <#iters> <naive|auto|sse|avx> "
            "<verify?>\n");
        exit(-1);
    }

    nx = strtol(argv[1], NULL, 10);
    ny = strtol(argv[2], NULL, 10);
    nz = strtol(argv[3], NULL, 10);

    padded_nx = ((nx + 0x7) & (~0x7));
    iters = strtol(argv[4], NULL, 10);

    if (strncasecmp(argv[5], "naive", 5) == 0)
        l3d = l3d_naive;
    else if (strncasecmp(argv[5], "auto", 4) == 0)
        l3d = l3d_auto;
    else if (strncasecmp(argv[5], "sse", 3) == 0)
        l3d = l3d_sse;
    else if (strncasecmp(argv[5], "avx", 3) == 0)
        l3d = l3d_avx;
    else {
        fprintf(stderr, "don't recognize %s. naive, auto, sse, or avx?\n",
                argv[5]);
        exit(-1);
    }

    verify = strtol(argv[6], NULL, 10);

    ghz = cpughz();
    nthreads = omp_get_max_threads();
    printf("machine speed is %g GHz, using %d threads\n", ghz, nthreads);

    printf("laplace3d: %d iterations on %dx%dx%d grid, "
           "verification is %s\n",
           iters, nx, ny, nz, verify ? "on" : "off");

    /* pad for aligned access; non-naive only */
    if (strncasecmp(argv[5], "naive", 5) != 0) {
        pad_size = (((1 + padded_nx + padded_nx * ny) + 0xF) & (~0xF)) -
                (1 + padded_nx + padded_nx * ny);
        printf("using padded_nx = %d, pad_size = %d\n",
               padded_nx, pad_size);

        u1_p = (float *)_mm_malloc(sizeof (float) *
                (padded_nx * ny * nz + pad_size), 64);
        u2_p = (float *)_mm_malloc(sizeof (float) *
                (padded_nx * ny * nz + pad_size), 64);
        u1 = u1_p + pad_size;
        u2 = u2_p + pad_size;
    }
    else {
        u1_p = (float *)_mm_malloc(sizeof (float) * (nx * ny * nz), 64);
        u2_p = (float *)_mm_malloc(sizeof (float) * (nx * ny * nz), 64);
        u1 = u1_p;
        u2 = u2_p;
        padded_nx = nx;
    }
    u1_orig = (float *)_mm_malloc(sizeof (float) * nx * ny * nz, 64);
    u2_orig = (float *)_mm_malloc(sizeof (float) * nx * ny * nz, 64);

    // initialize
    #pragma omp parallel for private(k,j,i,ind,p_ind)
    for (k = 0;  k < nz;  ++k) {
        for (j = 0;  j < ny;  ++j) {
            for (i = 0;  i < nx;  ++i) {
                ind = i + j*nx + k*nx*ny;
                p_ind = i + j*padded_nx + k*padded_nx*ny;

                if (i == 0  ||  i == nx - 1
                        ||  j == 0  ||  j == ny - 1
                        ||  k == 0  ||  k == nz - 1) {
                    // Dirichlet b.c.'s
                    u1[p_ind] = u1_orig[ind] = u2[p_ind] = 1.0f;
                }
                else {
                    u1[p_ind] = u1_orig[ind] = u2[p_ind] = 0.0f;
                }
            }
        }
    }

    // run optimized version
    uint64_t t0 = rdtsc();
    for (i = 0;  i < iters;  ++i) {
        l3d(nx, padded_nx, ny, nz, u1, u2);
        foo = u1; u1 = u2; u2 = foo;
    }
    uint64_t gold = rdtsc() - t0;
    double elapsed = gold / (ghz * 1e9);

    double grid_size = nx * ny * nz;
    double gflops = grid_size * iters * 6.0 / 1e9;
    double gflops_sec = gflops / elapsed;

    double traffic = grid_size * iters * 4 * 2.0 / 1e9;
    double bw_realized = traffic / elapsed;

    printf("laplace3d completed in %.4lf seconds\n", elapsed);
    printf("GFLOPs/sec: %.1f\n", gflops_sec);
    printf("BW realized: %.1f\n", bw_realized);

    if (verify) {
        // run serial version for verification
        uint64_t st0 = rdtsc();
        for (i = 0;  i < iters;  ++i) {
            l3d_orig(nx, ny, nz, u1_orig, u2_orig);
            foo = u1_orig; u1_orig = u2_orig; u2_orig = foo;
        }
        uint64_t ser = rdtsc() - st0;
        elapsed = ser / (ghz * 1e9);
        gflops_sec = gflops / elapsed;
        bw_realized = traffic / elapsed;

        printf("laplace3d_orig completed in %.2lf seconds\n", elapsed);
        printf("GFLOPs/sec: %.1f\n", gflops_sec);
        printf("BW realized: %.1f\n", bw_realized);

        // verify
        for (k = 0;  k < nz;  ++k) {
            for (j = 0;  j < ny;  ++j) {
                for (i = 0;  i < nx;  ++i) {
                    ind = i + j*nx + k*nx*ny;
                    p_ind = i + j*padded_nx + k*padded_nx*ny;

                    if (fabs(u1[p_ind] - u1_orig[ind]) > error_tol) {
                        printf("ERROR %f - %f [%d, %d, %d]\n",
                               u1[p_ind], u1_orig[ind], i, j, k);
                        goto done;
                    }
                }
            }
        }
        printf("verified, no error\n");
    }

    done:
    _mm_free(u1_p);
    _mm_free(u2_p);
    _mm_free(u1_orig);
    _mm_free(u2_orig);

    return 0;
}

void l3d_naive(int nx, int padded_nx, int ny, int nz, float *u1, float *u2)
{
    int i, j, k, ind;
    const float sixth = 1.0f/6.0f;

    /* compute on the grid */
    #pragma omp parallel for private(i,j,k,ind)
    for (k = 1;  k < nz-1;  ++k) {
        for (j = 1;  j < ny-1;  ++j) {
            #pragma ivdep
            for (i = 1;  i < nx-1;  ++i) {
                ind = i + j*padded_nx + k*padded_nx*ny;
                u2[ind] =
                   ( u1[ind-1    ]        + u1[ind+1    ]
                   + u1[ind-padded_nx   ] + u1[ind+padded_nx   ]
                   + u1[ind-padded_nx*ny] + u1[ind+padded_nx*ny] ) * sixth;
            }
        }
    }
}

void l3d_auto(int nx, int padded_nx, int ny, int nz, float *u1, float *u2)
{
    int i, j, k, ind;

    float sixth = 1.0f/6.0f;

#if defined(__INTEL_COMPILER)
    __assume(padded_nx%8==0);
    __assume_aligned(&u1[1],32);
    __assume_aligned(&u2[1],32);
#elif defined(__GNUC__)
    if (!(padded_nx%8==0))
        __builtin_unreachable();
    // third argument is the misalignment
    u1 = __builtin_assume_aligned(u1, 32, sizeof(float));
    u2 = __builtin_assume_aligned(u2, 32, sizeof(float));
#endif

    /* compute on the grid */
    #pragma omp parallel for private(i,j,k,ind)
    for (k = 1;  k < nz-1;  ++k) {
        for (j = 1;  j < ny-1;  ++j) {
            #pragma vector nontemporal(u2)
            for (i = 1;  i < nx-1;  ++i) {
                ind = i + j*padded_nx + k*padded_nx*ny;
                u2[ind] =
                    ( u1[ind-1    ]        + u1[ind+1    ]
                    + u1[ind-padded_nx   ] + u1[ind+padded_nx   ]
                    + u1[ind-padded_nx*ny] + u1[ind+padded_nx*ny] ) * sixth;
            }
        }
    }
}

void l3d_sse(int nx, int padded_nx, int ny, int nz, float *u1, float *u2)
{
    int i, j, k, ind;

    float fsixth = 1.0f/6.0f;
    __m128 sixth = _mm_set_ps1(fsixth);

    /* compute on the grid */
    #pragma omp parallel for private(i,j,k,ind)
    for (k = 1;  k < nz-1;  ++k) {
        for (j = 1;  j < ny-1;  ++j) {
            for (i = 1;  i < nx-1;  i += 4) {
                ind = i + j*padded_nx + k*padded_nx*ny;

                __m128 pSrc1 = _mm_loadu_ps(&u1[ind-1]);
                __m128 pSrc2 = _mm_loadu_ps(&u1[ind+1]);
                __m128 pSrc3 = _mm_load_ps(&u1[ind-padded_nx]);
                __m128 pSrc4 = _mm_load_ps(&u1[ind+padded_nx]);
                __m128 pSrc5 = _mm_load_ps(&u1[ind-padded_nx*ny]);
                __m128 pSrc6 = _mm_load_ps(&u1[ind+padded_nx*ny]);

                __m128 sum1 = _mm_add_ps(pSrc1, pSrc2);
                __m128 sum2 = _mm_add_ps(pSrc3, pSrc4);
                __m128 sum3 = _mm_add_ps(pSrc5, pSrc6);
                __m128 sum4 = _mm_add_ps(sum1, sum2);
                __m128 vsum = _mm_add_ps(sum3, sum4);

                vsum = _mm_mul_ps(vsum, sixth);

                _mm_stream_ps(&u2[ind], vsum);
            }
        }
    }
}

void l3d_avx(int nx, int padded_nx, int ny, int nz, float *u1, float *u2)
{
    int i, j, k, ind;

    float fsixth = 1.0f/6.0f;
    __m256 sixth = _mm256_set1_ps(fsixth);

    /* compute on the grid */
    #pragma omp parallel for private(i,j,k,ind)
    for (k = 1;  k < nz-1;  ++k) {
        for (j = 1;  j < ny-1;  ++j) {
            for (i = 1;  i < nx-1;  i += 8) {
                ind = i + j*padded_nx + k*padded_nx*ny;

                __m256 pSrc1 = _mm256_loadu_ps(&u1[ind-1]);
                __m256 pSrc2 = _mm256_loadu_ps(&u1[ind+1]);
                __m256 pSrc3 = _mm256_load_ps(&u1[ind-padded_nx]);
                __m256 pSrc4 = _mm256_load_ps(&u1[ind+padded_nx]);
                __m256 pSrc5 = _mm256_load_ps(&u1[ind-padded_nx*ny]);
                __m256 pSrc6 = _mm256_load_ps(&u1[ind+padded_nx*ny]);

                __m256 sum1 = _mm256_add_ps(pSrc1, pSrc2);
                __m256 sum2 = _mm256_add_ps(pSrc3, pSrc4);
                __m256 sum3 = _mm256_add_ps(pSrc5, pSrc6);
                __m256 sum4 = _mm256_add_ps(sum1, sum2);
                __m256 vsum = _mm256_add_ps(sum3, sum4);

                vsum = _mm256_mul_ps(vsum, sixth);

                _mm256_stream_ps(&u2[ind], vsum);
            }
        }
    }
}

void l3d_orig(int nx, int ny, int nz, float *u1, float *u2)
{
    int i, j, k, ind;
    const float sixth = 1.0f/6.0f;

    for (k = 0;  k < nz;  ++k) {
        for (j = 0;  j < ny;  ++j) {
            for (i = 0;  i < nx;  ++i) {
                ind = i + j*nx + k*nx*ny;

                if (i == 0  ||  i == nx - 1
                        ||  j == 0  ||  j == ny - 1
                        ||  k == 0  ||  k == nz - 1) {
                    u2[ind] = u1[ind];          // Dirichlet b.c.'s
                }
                else {
                    u2[ind] = ( u1[ind-1    ] + u1[ind+1    ]
                              + u1[ind-nx   ] + u1[ind+nx   ]
                              + u1[ind-nx*ny] + u1[ind+nx*ny] ) * sixth;
                }
            }
        }
    }
}
