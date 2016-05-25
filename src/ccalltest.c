// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <stdint.h>
#include <inttypes.h>

#include "../src/support/platform.h"
#include "../src/support/dtypes.h"

#ifdef _P64
#define jint int64_t
#define PRIjint PRId64
#else
#define jint int32_t
#define PRIjint PRId32
#endif

int verbose = 1;

int c_int = 0;

//////////////////////////////////
// Test for proper argument register truncation

int xs[300] = {0,0,0,1,0};

//int testUcharX(unsigned char x);
#ifdef _COMPILER_MICROSOFT_
int __declspec(noinline)
#else
int __attribute__((noinline))
#endif
JL_DLLEXPORT testUcharX(unsigned char x) {
    return xs[x];
}


//////////////////////////////////
// Tests for passing and returning Structs

// Complex-like data types
typedef struct {
    jint real;
    jint imag;
} complex_t;

JL_DLLEXPORT complex_t ctest(complex_t a) {
    a.real += 1;
    a.imag -= 2;
    return a;
}

JL_DLLEXPORT complex double cgtest(complex double a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(a), cimag(a));
    a += 1 - (2.0*I);
    return a;
}

JL_DLLEXPORT complex double *cgptest(complex double *a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - (2.0*I);
    return a;
}

JL_DLLEXPORT complex float cftest(complex float a) {
    //Unpack a ComplexPair{Float32} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(a), cimag(a));
    a += 1 - (2.0*I);
    return a;
}

JL_DLLEXPORT complex float *cfptest(complex float *a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - (2.0*I);
    return a;
}

JL_DLLEXPORT complex_t *cptest(complex_t *a) {
    //Unpack a ComplexPair{Int} struct pointer
    if (verbose) fprintf(stderr,"%" PRIjint " + %" PRIjint " i\n", a->real, a->imag);
    a->real += 1;
    a->imag -= 2;
    return a;
}

JL_DLLEXPORT complex_t *cptest_static(complex_t *a) {
    complex_t *b = (complex_t*)malloc(sizeof(complex_t));
    b->real = a->real;
    b->imag = a->imag;
    return b;
}

// Various sized data types
typedef struct {
    float x;
    double y;
} struct1;

typedef struct {
    struct { int32_t x; } x;
    struct { int32_t y; } y;
} struct2a;

typedef struct {
    int32_t x;
    int32_t y;
} struct2b;

typedef struct {
    struct { int64_t x; } x;
    struct { int64_t y; } y;
} struct3a;

typedef struct {
    int64_t x;
    int64_t y;
} struct3b;

typedef struct {
    int32_t x;
    int32_t y;
    int32_t z;
} struct4;

typedef struct {
    int32_t x;
    int32_t y;
    int32_t z;
    int32_t a;
} struct5;

typedef struct {
    int64_t x;
    int64_t y;
    int64_t z;
} struct6;

typedef struct {
    int64_t x;
    char y;
} struct7;

typedef struct {
    int32_t x;
    char y;
} struct8;

typedef struct {
    int32_t x;
    int16_t y;
} struct9;

typedef struct {
    char x;
    char y;
    char z;
    char a;
} struct10;

typedef struct {
    complex float x;
} struct11;

typedef struct {
    complex float x;
    complex float y;
} struct12;

typedef struct {
    complex double x;
} struct13;

typedef struct {
    float x;
    float y;
} struct14;

typedef struct {
    double x;
    double y;
} struct15;

typedef struct {
    float x,y,z;
    double a,b,c;
} struct16;

typedef struct {
    jint x;
    jint y;
    char z;
} struct_big;

typedef struct {
    int64_t r1;
    int64_t r2;
    int64_t r3;
    int64_t r4;
    int64_t r5;
    int64_t r6;
    int64_t r7;
    int64_t r8;
} struct_huge1a;

typedef struct {
    int64_t r1;
    int64_t r2;
    int64_t r3;
    int64_t r4;
    int64_t r5;
    int64_t r6;
    int64_t r7;
    int64_t r8;
    int64_t r9;
} struct_huge1b;

typedef struct {
    double f1;
    double f2;
    double f3;
    double f4;
    double f5;
    double f6;
    double f7;
    double f8;
} struct_huge2a;

typedef struct {
    double f1;
    double f2;
    double f3;
    double f4;
    double f5;
    double f6;
    double f7;
    double f8;
    double f9;
} struct_huge2b;

typedef struct {
    complex float f12;
    complex float f34;
    complex float f56;
    float f7;
    float f8;
} struct_huge3a;

typedef struct {
    complex float r1;
    complex float r2;
    complex float r3;
    complex float r4;
    complex float r5;
    complex float r6;
    complex float r7;
    float r8a;
    float r8b;
} struct_huge3b;

typedef struct {
    complex float r1;
    complex float r2;
    complex float r3;
    complex float r4;
    complex float r5;
    complex float r6;
    complex float r7;
    float r8a;
    float r8b;
    float r9;
} struct_huge3c;

typedef struct {
    complex double r12;
    complex double r34;
    complex float r5;
    complex double r67;
    double r8;
} struct_huge4a;

typedef struct {
    complex double r12;
    complex double r34;
    complex float r5;
    complex double r67;
    complex double r89;
} struct_huge4b;

typedef struct {
    complex int r1;
    complex int r2;
    complex int r3;
    complex int r4;
    complex int r5;
    complex int r6;
    complex int r7;
    complex int r8;
} struct_huge5a;

typedef struct {
    complex int r1;
    complex int r2;
    complex int r3;
    complex int r4;
    complex int r5;
    complex int r6;
    complex int r7;
    complex int r8;
    complex int r9;
} struct_huge5b;


JL_DLLEXPORT struct1 test_1(struct1 a, float b) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b * 1;
    a.y -= b * 2;
    return a;
}

JL_DLLEXPORT struct2a test_2a(struct2a a, int32_t b) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) fprintf(stderr,"%" PRId32 " + %" PRId32 " i & %" PRId32 "\n", a.x.x, a.y.y, b);
    a.x.x += b*1;
    a.y.y -= b*2;
    return a;
}

JL_DLLEXPORT struct2b test_2b(struct2b a, int32_t b) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) fprintf(stderr,"%" PRId32 " + %" PRId32 " i & %" PRId32 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct3a test_3a(struct3a a, int64_t b) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) fprintf(stderr,"%" PRId64 " + %" PRId64 " i & %" PRId64 "\n", a.x.x, a.y.y, b);
    a.x.x += b*1;
    a.y.y -= b*2;
    return a;
}

JL_DLLEXPORT struct3b test_3b(struct3b a, int64_t b) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) fprintf(stderr,"%" PRId64 " + %" PRId64 " i & %" PRId64 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct4 test_4(struct4 a, int32_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId32 ",%" PRId32 " & %" PRId32 "\n", a.x, a.y, a.z, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    return a;
}

JL_DLLEXPORT struct5 test_5(struct5 a, int32_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId32 ",%" PRId32 ",%" PRId32 " & %" PRId32 "\n", a.x, a.y, a.z, a.a, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    a.a -= b*4;

    return a;
}

JL_DLLEXPORT struct6 test_6(struct6 a, int64_t b) {
    if (verbose) fprintf(stderr,"%" PRId64 ",%" PRId64 ",%" PRId64 " & %" PRId64 "\n", a.x, a.y, a.z, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    return a;
}

JL_DLLEXPORT struct7 test_7(struct7 a, int8_t b) {
    if (verbose) fprintf(stderr,"%" PRId64 ",%" PRId8 " & %" PRId8 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct8 test_8(struct8 a, int8_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId8 " & %" PRId8 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct9 test_9(struct9 a, int16_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId16 " & %" PRId16 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct10 test_10(struct10 a, int8_t b) {
    if (verbose) fprintf(stderr,"%" PRId8 ",%" PRId8 ",%" PRId8 ",%" PRId8 " & %" PRId8 "\n", a.x, a.y, a.z, a.a, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    a.a -= b*4;

    return a;
}

JL_DLLEXPORT struct11 test_11(struct11 a, float b) {
    //Unpack a nested ComplexPair{Float32} struct
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", creal(a.x), cimag(a.x), b);
    a.x += b*1 - (b*2.0*I);
    return a;
}

JL_DLLEXPORT struct12 test_12(struct12 a, float b) {
    //Unpack two nested ComplexPair{Float32} structs
    if (verbose) fprintf(stderr,"%g + %g i & %g + %g i & %g\n",
                         creal(a.x), cimag(a.x), creal(a.y), cimag(a.y), b);
    a.x += b*1 - (b*2.0*I);
    a.y += b*3 - (b*4.0*I);
    return a;
}

JL_DLLEXPORT struct13 test_13(struct13 a, double b) {
    //Unpack a nested ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", creal(a.x), cimag(a.x), b);
    a.x += b*1 - (b*2.0*I);
    return a;
}

JL_DLLEXPORT struct14 test_14(struct14 a, float b) {
    //The C equivalent of a  ComplexPair{Float32} struct (but without special complex ABI)
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct15 test_15(struct15 a, double b) {
    //The C equivalent of a  ComplexPair{Float64} struct (but without special complex ABI)
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

JL_DLLEXPORT struct16 test_16(struct16 a, float b) {
    //Unpack a struct with non-obvious packing requirements
    if (verbose) fprintf(stderr,"%g %g %g %g %g %g & %g\n", a.x, a.y, a.z, a.a, a.b, a.c, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    a.a -= b*4;
    a.b += b*5;
    a.c -= b*6;
    return a;
}

// Note for AArch64:
// `i128` is a native type on aarch64 so the type here is wrong.
// However, it happens to have the same calling convention with `[2 x i64]`
// when used as first argument or return value.
#define int128_t struct3b
JL_DLLEXPORT int128_t test_128(int128_t a, int64_t b) {
    //Unpack a Int128
    if (verbose) fprintf(stderr,"0x%016" PRIx64 "%016" PRIx64 " & %" PRId64 "\n", a.y, a.x, b);
    a.x += b*1;
    if (a.x == 0)
        a.y += b*1;
    return a;
}

JL_DLLEXPORT struct_big test_big(struct_big a) {
    //Unpack a "big" struct { int, int, char }
    if (verbose) fprintf(stderr,"%" PRIjint " %" PRIjint " %c\n", a.x, a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z -= 'A';
    return a;
}

#define test_huge(suffix, reg) \
JL_DLLEXPORT struct_huge##suffix test_huge##suffix(char a, struct_huge##suffix b, char c) { \
    if (verbose) fprintf(stderr,"%c-%c\n", a, c); \
    b.reg *= 39; \
    return b; \
}

test_huge(1a, r1);
test_huge(1b, r1);
test_huge(2a, f1);
test_huge(2b, f1);
test_huge(3a, f12);
test_huge(3b, r1);
test_huge(3c, r1);
test_huge(4a, r12);
test_huge(4b, r12);
test_huge(5a, r1);
test_huge(5b, r1);

JL_DLLEXPORT int get_c_int(void)
{
    return c_int;
}

JL_DLLEXPORT void set_c_int(int i)
{
    c_int = i;
}

JL_DLLEXPORT void finalizer_cptr(void* v)
{
    set_c_int(-1);
}

//////////////////////////////////
// Turn off verbose for automated tests, leave on for debugging

JL_DLLEXPORT void set_verbose(int level) {
    verbose = level;
}

JL_DLLEXPORT void *test_echo_p(void *p) {
    return p;
}

#if defined(_CPU_X86_64_)

#include <xmmintrin.h>

JL_DLLEXPORT __m128i test_m128i(__m128i a, __m128i b, __m128i c, __m128i d )
{
    // 64-bit x86 has only level 2 SSE, which does not have a <4 x int32> multiplication,
    // so we use floating-point instead, and assume caller knows about the hack.
    return _mm_add_epi32(a,
                         _mm_cvtps_epi32(_mm_mul_ps(_mm_cvtepi32_ps(b),
                                                    _mm_cvtepi32_ps(_mm_sub_epi32(c,d)))));
}

JL_DLLEXPORT __m128 test_m128(__m128 a, __m128 b, __m128 c, __m128 d )
{
    return _mm_add_ps(a, _mm_mul_ps(b, _mm_sub_ps(c, d)));
}

#endif

#ifdef _CPU_AARCH64_

JL_DLLEXPORT __int128 test_aa64_i128_1(int64_t v1, __int128 v2)
{
    return v1 * 2 - v2;
}

typedef struct {
    int32_t v1;
    __int128 v2;
} struct_aa64_1;

JL_DLLEXPORT struct_aa64_1 test_aa64_i128_2(int64_t v1, __int128 v2,
                                            struct_aa64_1 v3)
{
    struct_aa64_1 x = {(int32_t)v1 / 2 + 1 - v3.v1, v2 * 2 - 1 - v3.v2};
    return x;
}

typedef struct {
    __fp16 v1;
    double v2;
} struct_aa64_2;

JL_DLLEXPORT __fp16 test_aa64_fp16_1(int v1, float v2, double v3, __fp16 v4)
{
    return (__fp16)(v1 + v2 * 2 + v3 * 3 + v4 * 4);
}

JL_DLLEXPORT struct_aa64_2 test_aa64_fp16_2(int v1, float v2,
                                            double v3, __fp16 v4)
{
    struct_aa64_2 x = {v4 / 2 + 1, v1 * 2 + v2 * 4 - v3};
    return x;
}

#include <arm_neon.h>

JL_DLLEXPORT int64x2_t test_aa64_vec_1(int32x2_t v1, float _v2, int32x2_t v3)
{
    int v2 = (int)_v2;
    return vmovl_s32(v1 * v2 + v3);
}

// This is a homogenious short vector aggregate
typedef struct {
    int8x8_t v1;
    float32x2_t v2;
} struct_aa64_3;

// This is NOT a homogenious short vector aggregate
typedef struct {
    float32x2_t v2;
    int16x8_t v1;
} struct_aa64_4;

JL_DLLEXPORT struct_aa64_3 test_aa64_vec_2(struct_aa64_3 v1, struct_aa64_4 v2)
{
    struct_aa64_3 x = {v1.v1 + vmovn_s16(v2.v1), v1.v2 - v2.v2};
    return x;
}

#endif
