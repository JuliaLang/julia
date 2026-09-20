// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "ccalltest_common.h"

int verbose = 1;
int c_int = 0;

//////////////////////////////////
// Test for proper argument register truncation

int xs[300] = {0,0,0,1,0};

//int testUcharX(unsigned char x);
int __attribute__((noinline))
DLLEXPORT testUcharX(unsigned char x) {
    return xs[x];
}


//////////////////////////////////
// Tests for passing and returning Structs

// Complex-like data types
typedef struct {
    jint real;
    jint imag;
} complex_t;

DLLEXPORT complex_t ctest(complex_t a) {
    a.real += 1;
    a.imag -= 2;
    return a;
}

DLLEXPORT complex double cgtest(complex double a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(a), cimag(a));
    a += 1 - (2.0*I);
    return a;
}

DLLEXPORT complex double *cgptest(complex double *a) {
    //Unpack a ComplexPair{Float64} struct pointer
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - (2.0*I);
    return a;
}

DLLEXPORT complex float cftest(complex float a) {
    //Unpack a ComplexPair{Float32} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(a), cimag(a));
    a += 1 - (2.0*I);
    return a;
}

DLLEXPORT complex float *cfptest(complex float *a) {
    //Unpack a ComplexPair{Float32} struct pointer
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - (2.0*I);
    return a;
}

DLLEXPORT complex_t *cptest(complex_t *a) {
    //Unpack a ComplexPair{Int} struct pointer
    if (verbose) fprintf(stderr,"%" PRIjint " + %" PRIjint " i\n", a->real, a->imag);
    a->real += 1;
    a->imag -= 2;
    return a;
}

DLLEXPORT complex_t *cptest_static(complex_t *a) {
    if (verbose) fprintf(stderr,"%" PRIjint " + %" PRIjint " i\n", a->real, a->imag);
    complex_t *b = (complex_t*)malloc_s(sizeof(complex_t));
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
    int8_t a;
    int16_t b;
} struct17;

typedef struct {
    int8_t a;
    int8_t b;
    int8_t c;
} struct18;

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

// Intel compiler does not currently support complex int (which is GNU extension)
#ifndef _COMPILER_INTEL_
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
#endif // _COMPILER_INTEL_


DLLEXPORT struct1 test_1(struct1 a, float b) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b * 1;
    a.y -= b * 2;
    return a;
}

DLLEXPORT struct1 test_1long_a(jint x1, jint x2, jint x3, struct1 a, float b) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"(%" PRIjint ", %" PRIjint ", %" PRIjint ") & %g + %g i & %g\n", x1, x2, x3, a.x, a.y, b);
    a.x += b + x1 + x2 + x3;
    a.y -= b * 2;
    return a;
}

DLLEXPORT struct1 test_1long_b(jint x1, double x2, jint x3, struct1 a, float b) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"(%" PRIjint ", %g, %" PRIjint ") & %g + %g i & %g\n", x1, x2, x3, a.x, a.y, b);
    a.x += b + x1 + x2 + x3;
    a.y -= b * 2;
    return a;
}

DLLEXPORT struct1 test_1long_c(jint x1, double x2, jint x3, jint x4, struct1 a, float b) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"(%" PRIjint ", %g, %" PRIjint ", %" PRIjint ") & %g + %g i & %g\n", x1, x2, x3, x4, a.x, a.y, b);
    a.x += b + x1 + x2 + x3 + x4;
    a.y -= b * 2;
    return a;
}

DLLEXPORT struct2a test_2a(struct2a a, int32_t b) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) fprintf(stderr,"%" PRId32 " + %" PRId32 " i & %" PRId32 "\n", a.x.x, a.y.y, b);
    a.x.x += b*1;
    a.y.y -= b*2;
    return a;
}

DLLEXPORT struct2b test_2b(struct2b a, int32_t b) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) fprintf(stderr,"%" PRId32 " + %" PRId32 " i & %" PRId32 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct3a test_3a(struct3a a, int64_t b) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) fprintf(stderr,"%" PRId64 " + %" PRId64 " i & %" PRId64 "\n", a.x.x, a.y.y, b);
    a.x.x += b*1;
    a.y.y -= b*2;
    return a;
}

DLLEXPORT struct3b test_3b(struct3b a, int64_t b) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) fprintf(stderr,"%" PRId64 " + %" PRId64 " i & %" PRId64 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct4 test_4(struct4 a, int32_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId32 ",%" PRId32 " & %" PRId32 "\n", a.x, a.y, a.z, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    return a;
}

DLLEXPORT struct5 test_5(struct5 a, int32_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId32 ",%" PRId32 ",%" PRId32 " & %" PRId32 "\n", a.x, a.y, a.z, a.a, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    a.a -= b*4;

    return a;
}

DLLEXPORT struct6 test_6(struct6 a, int64_t b) {
    if (verbose) fprintf(stderr,"%" PRId64 ",%" PRId64 ",%" PRId64 " & %" PRId64 "\n", a.x, a.y, a.z, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    return a;
}

DLLEXPORT struct7 test_7(struct7 a, int8_t b) {
    if (verbose) fprintf(stderr,"%" PRId64 ",%" PRId8 " & %" PRId8 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct8 test_8(struct8 a, int8_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId8 " & %" PRId8 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct9 test_9(struct9 a, int16_t b) {
    if (verbose) fprintf(stderr,"%" PRId32 ",%" PRId16 " & %" PRId16 "\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct10 test_10(struct10 a, int8_t b) {
    if (verbose) fprintf(stderr,"%" PRId8 ",%" PRId8 ",%" PRId8 ",%" PRId8 " & %" PRId8 "\n", a.x, a.y, a.z, a.a, b);
    a.x += b*1;
    a.y -= b*2;
    a.z += b*3;
    a.a -= b*4;

    return a;
}

DLLEXPORT struct11 test_11(struct11 a, float b) {
    //Unpack a nested ComplexPair{Float32} struct
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", creal(a.x), cimag(a.x), b);
    a.x += b*1 - (b*2.0*I);
    return a;
}

DLLEXPORT struct12 test_12(struct12 a, float b) {
    //Unpack two nested ComplexPair{Float32} structs
    if (verbose) fprintf(stderr,"%g + %g i & %g + %g i & %g\n",
                         creal(a.x), cimag(a.x), creal(a.y), cimag(a.y), b);
    a.x += b*1 - (b*2.0*I);
    a.y += b*3 - (b*4.0*I);
    return a;
}

DLLEXPORT struct13 test_13(struct13 a, double b) {
    //Unpack a nested ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", creal(a.x), cimag(a.x), b);
    a.x += b*1 - (b*2.0*I);
    return a;
}

DLLEXPORT struct14 test_14(struct14 a, float b) {
    //The C equivalent of a  ComplexPair{Float32} struct (but without special complex ABI)
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct15 test_15(struct15 a, double b) {
    //The C equivalent of a  ComplexPair{Float64} struct (but without special complex ABI)
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
    return a;
}

DLLEXPORT struct16 test_16(struct16 a, float b) {
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

DLLEXPORT struct17 test_17(struct17 a, int8_t b) {
    //Unpack a struct with non-obvious packing requirements
    if (verbose) fprintf(stderr,"%d %d & %d\n", (int)a.a, (int)a.b, (int)b);
    a.a += b*1;
    a.b -= b*2;
    return a;
}

DLLEXPORT struct18 test_18(struct18 a, int8_t b) {
    //Unpack a struct with non-obvious packing requirements
    if (verbose) fprintf(stderr,"%d %d %d & %d\n",
                         (int)a.a, (int)a.b, (int)a.c, (int)b);
    a.a += b*1;
    a.b -= b*2;
    a.c += b*3;
    return a;
}

// Note for AArch64:
// `i128` is a native type on aarch64 so the type here is wrong.
// However, it happens to have the same calling convention with `[2 x i64]`
// when used as first argument or return value.
#define int128_t struct3b
DLLEXPORT int128_t test_128(int128_t a, int64_t b) {
    //Unpack a Int128
    if (verbose) fprintf(stderr,"0x%016" PRIx64 "%016" PRIx64 " & %" PRId64 "\n", a.y, a.x, b);
    a.x += b*1;
    if (a.x == 0)
        a.y += b*1;
    return a;
}

DLLEXPORT struct_big test_big(struct_big a) {
    //Unpack a "big" struct { int, int, char }
    if (verbose) fprintf(stderr,"%" PRIjint " %" PRIjint " %c\n", a.x, a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z -= 'A';
    return a;
}

DLLEXPORT struct_big test_big_long(jint x1, jint x2, jint x3, struct_big a) {
    //Unpack a "big" struct { int, int, char }
    if (verbose) fprintf(stderr,"(%" PRIjint ", %" PRIjint ", %" PRIjint ") %" PRIjint " %" PRIjint " %c\n", x1, x2, x3, a.x, a.y, a.z);
    a.x += 1 + x1 + x2 + x3;
    a.y -= 2;
    a.z -= 'A';
    return a;
}

#define test_huge(suffix, reg) \
DLLEXPORT struct_huge##suffix test_huge##suffix(char a, struct_huge##suffix b, char c) { \
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
// Intel compiler does not currently support complex int (which is GNU extension)
#ifndef _COMPILER_INTEL_
test_huge(5a, r1);
test_huge(5b, r1);
#endif // _COMPILER_INTEL_

// Enough arguments for architectures that uses registers for integer or
// floating point arguments to spill.
DLLEXPORT int test_long_args_intp(int *a1, int *a2, int *a3, int *a4,
                                     int *a5, int *a6, int *a7, int *a8,
                                     int *a9, int *a10, int *a11, int *a12,
                                     int *a13, int *a14)
{
    return (*a1 + *a2 + *a3 + *a4 + *a5 + *a6 + *a7 + *a8 + *a9 + *a10 +
            *a11 + *a12 + *a13 + *a14);
}

DLLEXPORT int test_long_args_int(int a1, int a2, int a3, int a4,
                                    int a5, int a6, int a7, int a8,
                                    int a9, int a10, int a11, int a12,
                                    int a13, int a14)
{
    return (a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 +
            a11 + a12 + a13 + a14);
}

DLLEXPORT float test_long_args_float(float a1, float a2, float a3,
                                        float a4, float a5, float a6,
                                        float a7, float a8, float a9,
                                        float a10, float a11, float a12,
                                        float a13, float a14)
{
    return (a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 +
            a11 + a12 + a13 + a14);
}

DLLEXPORT double test_long_args_double(double a1, double a2, double a3,
                                          double a4, double a5, double a6,
                                          double a7, double a8, double a9,
                                          double a10, double a11, double a12,
                                          double a13, double a14)
{
    return (a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 +
            a11 + a12 + a13 + a14);
}

typedef struct {
    int *a;
    int *b;
} struct_spill_pint;

DLLEXPORT int test_spill_int1(int *v1, struct_spill_pint s)
{
    return *v1 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int2(int *v1, int *v2, struct_spill_pint s)
{
    return *v1 + *v2 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int3(int *v1, int *v2, int *v3, struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int4(int *v1, int *v2, int *v3, int *v4,
                                 struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *v4 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int5(int *v1, int *v2, int *v3, int *v4, int *v5,
                                 struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *v4 + *v5 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int6(int *v1, int *v2, int *v3, int *v4, int *v5,
                                 int *v6, struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *v4 + *v5 + *v6 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int7(int *v1, int *v2, int *v3, int *v4, int *v5,
                                 int *v6, int *v7, struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *v4 + *v5 + *v6 + *v7 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int8(int *v1, int *v2, int *v3, int *v4, int *v5,
                                 int *v6, int *v7, int *v8, struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *v4 + *v5 + *v6 + *v7 + *v8 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int9(int *v1, int *v2, int *v3, int *v4, int *v5,
                                 int *v6, int *v7, int *v8, int *v9,
                                 struct_spill_pint s)
{
    return *v1 + *v2 + *v3 + *v4 + *v5 + *v6 + *v7 + *v8 + *v9 + *s.a + *s.b;
}

DLLEXPORT int test_spill_int10(int *v1, int *v2, int *v3, int *v4, int *v5,
                                  int *v6, int *v7, int *v8, int *v9, int *v10,
                                  struct_spill_pint s)
{
    return (*v1 + *v2 + *v3 + *v4 + *v5 + *v6 + *v7 + *v8 + *v9 + *v10 +
            *s.a + *s.b);
}

typedef struct {
    float a;
    float b;
} struct_spill_float;

DLLEXPORT float test_spill_float1(float v1, struct_spill_float s)
{
    return v1 + s.a + s.b;
}

DLLEXPORT float test_spill_float2(float v1, float v2, struct_spill_float s)
{
    return v1 + v2 + s.a + s.b;
}

DLLEXPORT float test_spill_float3(float v1, float v2, float v3,
                                     struct_spill_float s)
{
    return v1 + v2 + v3 + s.a + s.b;
}

DLLEXPORT float test_spill_float4(float v1, float v2, float v3, float v4,
                                     struct_spill_float s)
{
    return v1 + v2 + v3 + v4 + s.a + s.b;
}

DLLEXPORT float test_spill_float5(float v1, float v2, float v3, float v4,
                                     float v5, struct_spill_float s)
{
    return v1 + v2 + v3 + v4 + v5 + s.a + s.b;
}

DLLEXPORT float test_spill_float6(float v1, float v2, float v3, float v4,
                                     float v5, float v6, struct_spill_float s)
{
    return v1 + v2 + v3 + v4 + v5 + v6 + s.a + s.b;
}

DLLEXPORT float test_spill_float7(float v1, float v2, float v3, float v4,
                                     float v5, float v6, float v7,
                                     struct_spill_float s)
{
    return v1 + v2 + v3 + v4 + v5 + v6 + v7 + s.a + s.b;
}

DLLEXPORT float test_spill_float8(float v1, float v2, float v3, float v4,
                                     float v5, float v6, float v7, float v8,
                                     struct_spill_float s)
{
    return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + s.a + s.b;
}

DLLEXPORT float test_spill_float9(float v1, float v2, float v3, float v4,
                                     float v5, float v6, float v7, float v8,
                                     float v9, struct_spill_float s)
{
    return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + s.a + s.b;
}

DLLEXPORT float test_spill_float10(float v1, float v2, float v3, float v4,
                                      float v5, float v6, float v7, float v8,
                                      float v9, float v10, struct_spill_float s)
{
    return (v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + s.a + s.b);
}

DLLEXPORT int get_c_int(void)
{
    return c_int;
}

DLLEXPORT void set_c_int(int i)
{
    c_int = i;
}

DLLEXPORT void finalizer_cptr(void* v)
{
    set_c_int(-1);
}


//////////////////////////////////
// Turn off verbose for automated tests, leave on for debugging

DLLEXPORT void set_verbose(int level) {
    verbose = level;
}


//////////////////////////////////
// Other tests

DLLEXPORT void *test_echo_p(void *p) {
    return p;
}

#if defined(_CPU_X86_64_)

#include <xmmintrin.h>

DLLEXPORT __m128i test_m128i(__m128i a, __m128i b, __m128i c, __m128i d)
{
    // 64-bit x86 has only level 2 SSE, which does not have a <4 x int32> multiplication,
    // so we use floating-point instead, and assume caller knows about the hack.
    return _mm_add_epi32(a,
                         _mm_cvtps_epi32(_mm_mul_ps(_mm_cvtepi32_ps(b),
                                                    _mm_cvtepi32_ps(_mm_sub_epi32(c,d)))));
}

DLLEXPORT __m128 test_m128(__m128 a, __m128 b, __m128 c, __m128 d)
{
    return _mm_add_ps(a, _mm_mul_ps(b, _mm_sub_ps(c, d)));
}

#endif

#ifdef _CPU_AARCH64_

DLLEXPORT __int128 test_aa64_i128_1(int64_t v1, __int128 v2)
{
    return v1 * 2 - v2;
}

typedef struct {
    int32_t v1;
    __int128 v2;
} struct_aa64_1;

DLLEXPORT struct_aa64_1 test_aa64_i128_2(int64_t v1, __int128 v2,
                                            struct_aa64_1 v3)
{
    struct_aa64_1 x = {(int32_t)v1 / 2 + 1 - v3.v1, v2 * 2 - 1 - v3.v2};
    return x;
}

typedef struct {
    __fp16 v1;
    double v2;
} struct_aa64_2;

DLLEXPORT __fp16 test_aa64_fp16_1(int v1, float v2, double v3, __fp16 v4)
{
    return (__fp16)(v1 + v2 * 2 + v3 * 3 + v4 * 4);
}

DLLEXPORT struct_aa64_2 test_aa64_fp16_2(int v1, float v2,
                                            double v3, __fp16 v4)
{
    struct_aa64_2 x = {v4 / 2 + 1, v1 * 2 + v2 * 4 - v3};
    return x;
}

#include <arm_neon.h>

DLLEXPORT int64x2_t test_aa64_vec_1(int32x2_t v1, float _v2, int32x2_t v3)
{
    int v2 = (int)_v2;
    return vmovl_s32(v1 * v2 + v3);
}

// This is a homogeneous short vector aggregate
typedef struct {
    int8x8_t v1;
    float32x2_t v2;
} struct_aa64_3;

// This is NOT a homogeneous short vector aggregate
typedef struct {
    float32x2_t v2;
    int16x8_t v1;
} struct_aa64_4;

DLLEXPORT struct_aa64_3 test_aa64_vec_2(struct_aa64_3 v1, struct_aa64_4 v2)
{
    // The cast below is to workaround GCC issue.
    // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96990
    struct_aa64_3 x = {(int8x8_t)(v1.v1 + vmovn_s16(v2.v1)), (float32x2_t)(v1.v2 - v2.v2)};
    return x;
}

#endif

#if defined(_CPU_PPC64_)

typedef int32_t int32x2_t __attribute__ ((vector_size (8)));
typedef float float32x2_t __attribute__ ((vector_size (8)));
typedef int32_t int32x4_t __attribute__ ((vector_size (16)));
typedef float float32x4_t __attribute__ ((vector_size (16)));
typedef double float64x2_t __attribute__ ((vector_size (16)));

typedef struct {
    int64_t m;
    float32x4_t v;
} struct_huge1_ppc64;

typedef struct {
    float32x4_t v1;
    int32x2_t v2;
} struct_huge2_ppc64;

typedef struct {
    float32x4_t v1;
    struct {
        float f1;
        float f2;
        float f3;
        float f4;
    };
} struct_huge3_ppc64;

typedef struct {
    float32x2_t v1;
    float64x2_t v2;
} struct_huge4_ppc64;

typedef struct {
    float32x4_t v1[9];
} struct_huge5_ppc64;

typedef struct {
    float32x4_t v1[8];
    float32x4_t v2;
} struct_huge6_ppc64;

typedef struct {
    float32x4_t v1[8];
} struct_huge1_ppc64_hva;

typedef struct {
    struct {
        float32x4_t vf[2];
    } v[2];
} struct_huge2_ppc64_hva;

typedef struct {
    float32x4_t vf1;
    struct {
        float32x4_t vf2[2];
    };
} struct_huge3_ppc64_hva;

typedef struct {
    int32x4_t v1;
    float32x4_t v2;
} struct_huge4_ppc64_hva;

typedef struct {
    float32x4_t v1;
    float64x2_t v2;
} struct_huge5_ppc64_hva;

test_huge(1_ppc64, m);
test_huge(2_ppc64, v1[0]);
test_huge(3_ppc64, v1[0]);
test_huge(4_ppc64, v1[0]);
test_huge(5_ppc64, v1[0][0]);
test_huge(6_ppc64, v1[0][0]);
test_huge(1_ppc64_hva, v1[0][0]);
test_huge(2_ppc64_hva, v[0].vf[0][0]);
test_huge(3_ppc64_hva, vf1[0]);
test_huge(4_ppc64_hva, v1[0]);
test_huge(5_ppc64_hva, v1[0]);

DLLEXPORT int64_t test_ppc64_vec1long(
        int64_t d1, int64_t d2, int64_t d3, int64_t d4, int64_t d5, int64_t d6,
        int64_t d7, int64_t d8, int64_t d9, struct_huge1_ppc64 vs)
{
    return d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + vs.m + vs.v[0] + vs.v[1] + vs.v[2] + vs.v[3];
}

DLLEXPORT int64_t test_ppc64_vec1long_vec(
        int64_t d1, int64_t d2, int64_t d3, int64_t d4, int64_t d5, int64_t d6,
        int64_t d7, int64_t d8, int64_t d9, float32x4_t vs)
{
    return d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + vs[0] + vs[1] + vs[2] + vs[3];
}

DLLEXPORT float32x4_t test_ppc64_vec2(int64_t d1, float32x4_t a, float32x4_t b, float32x4_t c, float32x4_t d,
                                         float32x4_t e, float32x4_t f, float32x4_t g, float32x4_t h, float32x4_t i,
                                         float32x4_t j, float32x4_t k, float32x4_t l, float32x4_t m, float32x4_t n)
{
    float32x4_t r;
    r[0] = d1 + a[0] + b[0] + c[0] + d[0] + e[0] + f[0] + g[0] + h[0] + i[0] + j[0] + k[0] + l[0] + m[0] + n[0];
    r[1] = d1 + a[1] + b[1] + c[1] + d[1] + e[1] + f[1] + g[1] + h[1] + i[1] + j[1] + k[1] + l[1] + m[1] + n[1];
    r[2] = d1 + a[2] + b[2] + c[2] + d[2] + e[2] + f[2] + g[2] + h[2] + i[2] + j[2] + k[2] + l[2] + m[2] + n[2];
    r[3] = d1 + a[3] + b[3] + c[3] + d[3] + e[3] + f[3] + g[3] + h[3] + i[3] + j[3] + k[3] + l[3] + m[3] + n[3];
    return r;
}

#endif

DLLEXPORT int threadcall_args(int a, int b) {
    return a + b;
}

DLLEXPORT void c_exit_finalizer(void* v) {
    printf("c_exit_finalizer: %d, %u", *(int*)v, (unsigned)((uintptr_t)v & (uintptr_t)1));
}

// Test helpers for `@ccall cancel_handler=(fn, state) ...`: a foreign
// computation that spins until cancelled, and a matching handler.
// `cell` points at two int64 slots: [0] the stop flag, [1] a scratch slot
// the handler fills. The handler runs asynchronously on the thread blocked
// in cancelspin_wait, like a signal handler.
DLLEXPORT int64_t cancelspin_wait(volatile int64_t *cell) {
    int64_t iters = 0;
    while (!cell[0])
        iters++;
    return iters;
}

// The handler runs under signal-handler discipline; the delivery machinery
// preserves the complete interrupted register state around it.
DLLEXPORT void cancelspin_handler(void *state, uint8_t sev) {
    volatile int64_t *cell = (volatile int64_t*)state;
    cell[1] = (int64_t)sev + 1; // distinguish sev 0 from "never ran"
    cell[0] = 1;
}

// Use a wider prototype to observe the caller's XLEN extension.
DLLEXPORT int64_t test_reg_32(int64_t x) {
    return x;
}

DLLEXPORT int64_t test_reg_32_va(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int64_t x = va_arg(ap, int64_t);
    va_end(ap);
    return x;
}

// Exercise ABI classification at register boundaries; sentinels detect miscounts.
typedef struct {
    int64_t i[8];
    double d[8];
    int64_t s1;
    double s2;
    char buf[32];
} abi_probe_t;
DLLEXPORT abi_probe_t abi_probe;
// GCC and Clang disagree on one-element floating-point vectors on x86-64.
#ifdef __clang__
DLLEXPORT int abi_probe_clang = 1;
#else
DLLEXPORT int abi_probe_clang = 0;
#endif
// GCC before 8.4 and 9.3 loads a variadic `__int128` from the register save
// area with an aligned 16-byte load, which faults when the value starts at an
// odd register slot (e.g. `rsi:rdx`).
#if defined(_CPU_X86_64_) && !defined(_OS_WINDOWS_) && defined(__GNUC__) && !defined(__clang__) && \
    (__GNUC__ < 8 || (__GNUC__ == 8 && __GNUC_MINOR__ < 4) || \
     (__GNUC__ == 9 && __GNUC_MINOR__ < 3))
DLLEXPORT int abi_probe_va_i128_regs = 0;
#else
DLLEXPORT int abi_probe_va_i128_regs = 1;
#endif

#define ABI_P0I
#define ABI_P0D
#define ABI_A0I
#define ABI_A0D
#define ABI_S0I
#define ABI_S0D
#define ABI_P5I int64_t i0, int64_t i1, int64_t i2, int64_t i3, int64_t i4,
#define ABI_P7I ABI_P5I int64_t i5, int64_t i6,
#define ABI_P8I ABI_P7I int64_t i7,
#define ABI_P7D double d0, double d1, double d2, double d3, double d4, double d5, double d6,
#define ABI_P8D ABI_P7D double d7,
// Values used by the C caller.
#define ABI_A5I 1000, 1001, 1002, 1003, 1004,
#define ABI_A7I ABI_A5I 1005, 1006,
#define ABI_A8I ABI_A7I 1007,
#define ABI_A7D 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5,
#define ABI_A8D ABI_A7D 7.5,
#define ABI_S5I abi_probe.i[0] = i0; abi_probe.i[1] = i1; abi_probe.i[2] = i2; abi_probe.i[3] = i3; \
                abi_probe.i[4] = i4;
#define ABI_S7I ABI_S5I abi_probe.i[5] = i5; abi_probe.i[6] = i6;
#define ABI_S8I ABI_S7I abi_probe.i[7] = i7;
#define ABI_S7D abi_probe.d[0] = d0; abi_probe.d[1] = d1; abi_probe.d[2] = d2; abi_probe.d[3] = d3; \
                abi_probe.d[4] = d4; abi_probe.d[5] = d5; abi_probe.d[6] = d6;
#define ABI_S8D ABI_S7D abi_probe.d[7] = d7;

#define ABI_PROBE(name, T, G, F) \
    DLLEXPORT void test_abi_arg_##name##_##G##_##F(ABI_P##G##I ABI_P##F##D T x, int64_t s1, double s2) { \
        ABI_S##G##I ABI_S##F##D \
        memcpy(abi_probe.buf, &x, sizeof(x)); abi_probe.s1 = s1; abi_probe.s2 = s2; } \
    DLLEXPORT T test_abi_ret_##name##_##G##_##F(ABI_P##G##I ABI_P##F##D int64_t s1) { \
        ABI_S##G##I ABI_S##F##D abi_probe.s1 = s1; \
        T r; memcpy(&r, abi_probe.buf, sizeof(r)); return r; } \
    DLLEXPORT void test_abi_cbarg_##name##_##G##_##F(void (*cb)(ABI_P##G##I ABI_P##F##D T, int64_t, double)) { \
        T x; memcpy(&x, abi_probe.buf, sizeof(x)); cb(ABI_A##G##I ABI_A##F##D x, -77, 3.25); } \
    DLLEXPORT void test_abi_cbret_##name##_##G##_##F(T (*cb)(ABI_P##G##I ABI_P##F##D int64_t)) { \
        T r = cb(ABI_A##G##I ABI_A##F##D -77); memcpy(abi_probe.buf, &r, sizeof(r)); }
// Vary the leading arguments to exercise register-pair alignment and spills.
#define ABI_VA(name, T, G) \
    DLLEXPORT void test_abi_va_##name##_##G(int n, ...) { \
        va_list ap; va_start(ap, n); \
        for (int k = 0; k < G; k++) abi_probe.i[k] = va_arg(ap, int64_t); \
        T x = va_arg(ap, T); memcpy(abi_probe.buf, &x, sizeof(x)); \
        abi_probe.s1 = va_arg(ap, int64_t); abi_probe.s2 = va_arg(ap, double); va_end(ap); }
#define ABI_BASIC(name, T) ABI_PROBE(name, T, 0, 0)
#define ABI_FP1(name, T) ABI_BASIC(name, T) ABI_PROBE(name, T, 0, 8)
#define ABI_FP2(name, T) \
    ABI_BASIC(name, T) ABI_PROBE(name, T, 0, 7) ABI_PROBE(name, T, 0, 8)
#define ABI_MIXED(name, T) \
    ABI_BASIC(name, T) ABI_PROBE(name, T, 7, 7) \
    ABI_PROBE(name, T, 8, 7) ABI_PROBE(name, T, 7, 8)
#define ABI_I128(name, T) \
    ABI_BASIC(name, T) ABI_PROBE(name, T, 5, 0) \
    ABI_PROBE(name, T, 7, 0) ABI_PROBE(name, T, 8, 0)
#define ABI_VA_BASIC(name, T) ABI_VA(name, T, 0)
#define ABI_VA_I128(name, T) \
    ABI_VA_BASIC(name, T) ABI_VA(name, T, 4) ABI_VA(name, T, 5) ABI_VA(name, T, 7)

#ifdef _P64
typedef struct { float x; } abi_S_f;
typedef struct { double x; } abi_S_d;
typedef struct { abi_S_d s; } abi_N_d;
typedef struct { double v[1]; } abi_T1d;
typedef struct { float a; float b; } abi_S_ff;
typedef struct { float a; double b; } abi_S_fd;
typedef struct { float v[2]; } abi_T2f;
typedef struct { float a; int32_t b; } abi_S_fi;
typedef struct { int32_t a; float b; } abi_S_if;
typedef struct { double a; int64_t b; } abi_S_di;
typedef struct { abi_S_f s; int32_t i; } abi_M_fi;
typedef struct { int64_t x; } abi_S_l;
typedef struct { double d; abi_S_l s; } abi_M_dl;
typedef struct { float a; void *b; } abi_S_fp;
typedef double abi_V1d __attribute__((vector_size(8)));
typedef float abi_V2f __attribute__((vector_size(8)));
typedef int32_t abi_V2i __attribute__((vector_size(8)));
typedef int64_t abi_V1l __attribute__((vector_size(8)));
typedef float abi_V1f __attribute__((vector_size(4)));
typedef double abi_V2d __attribute__((vector_size(16)));
typedef struct { double d; abi_V1d v; } abi_S_dV1;
typedef struct { double d; abi_V2i v; } abi_S_dV2i;
typedef struct { abi_V1f v; } abi_S_V1f;
typedef struct { __int128 x; } abi_A16;
ABI_FP1(Float32, float)
ABI_FP1(Float64, double)
ABI_I128(Int128, __int128)
ABI_BASIC(Ptr, void*)
ABI_FP1(S_f, abi_S_f)
ABI_BASIC(S_d, abi_S_d)
ABI_BASIC(N_d, abi_N_d)
ABI_BASIC(T1d, abi_T1d)
ABI_FP2(S_ff, abi_S_ff)
ABI_FP2(S_fd, abi_S_fd)
ABI_BASIC(T2f, abi_T2f)
ABI_MIXED(S_fi, abi_S_fi)
ABI_BASIC(S_if, abi_S_if)
ABI_BASIC(S_di, abi_S_di)
ABI_BASIC(M_fi, abi_M_fi)
ABI_BASIC(S_l, abi_S_l)
ABI_BASIC(M_dl, abi_M_dl)
ABI_BASIC(S_fp, abi_S_fp)
ABI_BASIC(V1d, abi_V1d)
ABI_BASIC(V2f, abi_V2f)
ABI_BASIC(V2i, abi_V2i)
ABI_BASIC(V1l, abi_V1l)
ABI_BASIC(V1f, abi_V1f)
ABI_BASIC(V2d, abi_V2d)
ABI_BASIC(S_dV1, abi_S_dV1)
ABI_BASIC(S_dV2i, abi_S_dV2i)
ABI_BASIC(S_V1f, abi_S_V1f)
ABI_I128(A16, abi_A16)
ABI_VA_I128(Int128, __int128)
ABI_VA_BASIC(S_f, abi_S_f)
ABI_VA_BASIC(S_ff, abi_S_ff)
ABI_VA_BASIC(S_fi, abi_S_fi)
ABI_VA_I128(A16, abi_A16)
// Unions and pointer fields use the integer convention.
typedef struct { double x; int32_t y; uint8_t sel; } abi_SU;
typedef struct { double x; void *y; } abi_SA;
DLLEXPORT double test_abi_union(abi_SU s) { memcpy(abi_probe.buf, &s, sizeof(s)); return s.x; }
DLLEXPORT double test_abi_boxed(abi_SA s) { abi_probe.s1 = (int64_t)(uintptr_t)s.y; return s.x; }
#endif
// These bit-copying probes are also used for `BFloat16`.
#if defined(__FLT16_MANT_DIG__)
DLLEXPORT int abi_probe_float16 = 1;
typedef struct { _Float16 x; } abi_S_h;
typedef struct { _Float16 a; _Float16 b; } abi_S_hh;
typedef struct { _Float16 a; double b; } abi_S_hd;
typedef struct { _Float16 a; int32_t b; } abi_S_hi;
ABI_FP1(Float16, _Float16)
ABI_BASIC(S_h, abi_S_h)
ABI_FP2(S_hh, abi_S_hh)
ABI_FP2(S_hd, abi_S_hd)
ABI_MIXED(S_hi, abi_S_hi)
// `_Float16` is not promoted through `...`.
ABI_VA_BASIC(Float16, _Float16)
#else
DLLEXPORT int abi_probe_float16 = 0;
#endif

// global variable for cglobal testing
DLLEXPORT const int global_var = 1;
