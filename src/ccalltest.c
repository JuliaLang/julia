// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <stdint.h>
#include <inttypes.h>
int verbose = 1;

#include "../src/support/platform.h"
#include "../src/support/dtypes.h"

#ifdef _P64
#define jint int64_t
#else
#define jint int32_t
#endif

//////////////////////////////////
// Test for proper argument register truncation

int xs[300] = {0,0,0,1,0};

//int testUcharX(unsigned char x);
#ifdef _COMPILER_MICROSOFT_
int __declspec(noinline)
#else
int __attribute__((noinline))
#endif
DLLEXPORT testUcharX(unsigned char x) {
    return xs[x];
}

#define xstr(s) str(s)
#define str(s) #s
int (*volatile fptr)(unsigned char x);
volatile int a;
volatile int b;

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

DLLEXPORT complex double* cgptest(complex double *a) {
    //Unpack a ComplexPair{Float64} struct
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

DLLEXPORT complex float* cfptest(complex float *a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) fprintf(stderr,"%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - (2.0*I);
    return a;
}

DLLEXPORT complex_t* cptest(complex_t *a) {
    //Unpack a ComplexPair{Int} struct pointer
    if (verbose) fprintf(stderr,"%lld + %lld i\n", (long long)a->real, (long long)a->imag);
    a->real += 1;
    a->imag -= 2;
    return a;
}

DLLEXPORT complex_t* cptest_static(complex_t *a) {
    complex_t *b = (complex_t*)malloc(sizeof(complex_t));
    b->real = a->real;
    b->imag = a->imag;
    return b;
}

// Native-like data types
DLLEXPORT char* stest(char *x) {
    //Print a character Array
    if (verbose) fprintf(stderr,"%s\n", x);
    return x;
}

struct jl_asciistring_t { struct { void* type; char* data; } *data; };
char* sptest(struct jl_asciistring_t str) {
    //Unpack an ASCIIString
    return stest(str.data->data);
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
    jint x;
    jint y;
    char z;
} struct_big;

DLLEXPORT struct1 test_1(struct1 a) {
    //Unpack a "small" struct { float, double }
    if (verbose) fprintf(stderr,"%g + %g i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct1 add_1(struct1 a, struct1 b) {
    // Two small structs
    struct1 c;
    c.x = a.x + b.x;
    c.y = a.y + b.y;
    return c;
}

DLLEXPORT struct2a test_2a(struct2a a) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) fprintf(stderr,"%" PRId32 " + %" PRId32 " i\n", a.x.x, a.y.y);
    a.x.x += 1;
    a.y.y -= 2;
    return a;
}

DLLEXPORT struct2b test_2b(struct2b a) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) fprintf(stderr,"%" PRId32 " + %" PRId32 " i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct3a test_3a(struct3a a) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) fprintf(stderr,"%" PRId64 " + %" PRId64 " i\n", a.x.x, a.y.y);
    a.x.x += 1;
    a.y.y -= 2;
    return a;
}

DLLEXPORT struct3b test_3b(struct3b a) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) fprintf(stderr,"%" PRId64 " + %" PRId64 " i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct4 test_4(struct4 a)
{
    if (verbose) fprintf(stderr,"(%" PRId32 ",%" PRId32 ",%" PRId32 ")\n", a.x, a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z += 3;
    return a;
}


DLLEXPORT struct5 test_5(struct5 a)
{
    if (verbose) fprintf(stderr,"(%" PRId32 ",%" PRId32 ",%" PRId32 ",%" PRId32 ")\n", a.x, a.y, a.z, a.a);
    a.x += 1;
    a.y -= 2;
    a.z += 3;
    a.a -= 4;

    return a;
}


DLLEXPORT struct6 test_6(struct6 a)
{
    if (verbose) fprintf(stderr,"(%" PRId64 ",%" PRId64 ",%" PRId64 ")\n", a.x, a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z += 3;
    return a;
}

DLLEXPORT struct7 test_7(struct7 a)
{
    if (verbose) fprintf(stderr,"(%" PRId64 ",%" PRId8 ")\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct8 test_8(struct8 a)
{
    if (verbose) fprintf(stderr,"(%" PRId32 ",%" PRId8 ")\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct9 test_9(struct9 a)
{
    if (verbose) fprintf(stderr,"(%" PRId32 ",%" PRId16 ")\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct10 test_10(struct10 a)
{
    if (verbose) fprintf(stderr,"(%" PRId8 ",%" PRId8 ",%" PRId8 ",%" PRId8 ")\n", a.x, a.y, a.z, a.a);
    a.x += 1;
    a.y -= 2;
    a.z += 3;
    a.a -= 4;

    return a;
}

DLLEXPORT struct14 test_14(struct14 a) {
    //The C equivalent of a  ComplexPair{Float32} struct (but without special complex ABI)
    if (verbose) fprintf(stderr,"%g + %g i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

DLLEXPORT struct15 test_15(struct15 a) {
    //The C equivalent of a  ComplexPair{Float32} struct (but without special complex ABI)
    if (verbose) fprintf(stderr,"%g + %g i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

#define int128_t struct3b
DLLEXPORT int128_t test_128(int128_t a) {
    //Unpack a Int128
    if (verbose) fprintf(stderr,"0x%016" PRIx64 "%016" PRIx64 "\n", a.y, a.x);
    a.x += 1;
    if (a.x == 0)
        a.y += 1;
    return a;
}

DLLEXPORT struct_big test_big(struct_big a) {
    //Unpack a "big" struct { int, int, char }
    if (verbose) fprintf(stderr,"%lld %lld %c\n", (long long)a.x, (long long)a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z -= 'A';
    return a;
}

int main() {
    fprintf(stderr,"all of the following should be 1 except xs[259] = 0\n");
    a = 3;
    b = 259;
    fptr = (int (*)(unsigned char x))&testUcharX;
    if ((((size_t)fptr)&((size_t)1)) == 1) fptr = NULL;
    fprintf(stderr,"compiled with: '%s'\nxs[3] = %d\nxs[259] = %d\ntestUcharX(3) = %d\ntestUcharX(%d) = %d\nfptr(3) = %d\nfptr(259) = %d\n",
           xstr(CC), xs[a], xs[b], testUcharX(a), b, testUcharX((unsigned char)b), fptr(a), fptr(b));
    fprintf(stderr,"misc tests:\n");
    struct1 a = {352.39422e23, 19.287577};
    a = test_1(a);
}

//////////////////////////////////
// Turn off verbose for automated tests, leave on for debugging
DLLEXPORT void set_verbose(int level) {
    verbose = level;
}

DLLEXPORT void *test_echo_p(void *p) {
    return p;
}
