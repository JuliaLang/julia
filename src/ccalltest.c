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
#else
#define jint int32_t
#endif

int verbose = 1;


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

JL_DLLEXPORT complex double* cgptest(complex double *a) {
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

JL_DLLEXPORT complex float* cfptest(complex float *a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - (2.0*I);
    return a;
}

JL_DLLEXPORT complex_t* cptest(complex_t *a) {
    //Unpack a ComplexPair{Int} struct pointer
    if (verbose) fprintf(stderr,"%lld + %lld i\n", (long long)a->real, (long long)a->imag);
    a->real += 1;
    a->imag -= 2;
    return a;
}

JL_DLLEXPORT complex_t* cptest_static(complex_t *a) {
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

JL_DLLEXPORT struct1 test_1(struct1 a, float b) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"%g + %g i & %g\n", a.x, a.y, b);
    a.x += b*1;
    a.y -= b*2;
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
    if (verbose) fprintf(stderr,"%lld %lld %c\n", (long long)a.x, (long long)a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z -= 'A';
    return a;
}


//////////////////////////////////
// Turn off verbose for automated tests, leave on for debugging

JL_DLLEXPORT void set_verbose(int level) {
    verbose = level;
}

JL_DLLEXPORT void *test_echo_p(void *p) {
    return p;
}
