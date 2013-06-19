#include <stdio.h>
#include <complex.h>
#include <stdint.h>
#include <inttypes.h>
int verbose = 1;

//////////////////////////////////
// Test for proper argument register truncation

int xs[300] = {0,0,0,1,0};

//int testUcharX(unsigned char x);
int __attribute__((noinline)) testUcharX(unsigned int x) {
    return xs[x];
}

#define xstr(s) str(s)
#define str(s) #s
volatile int (*fptr)(unsigned char x);
volatile int a;
volatile int b;

int main() {
    printf("all of the following should be 1 except xs[259] = 0");
    a = 3;
    b = 259;
    fptr = (volatile int (*)(unsigned char x))&testUcharX;
    if ((((long)fptr)&((long)1)<<32) == 1) fptr = NULL;
    printf("compiled with: '%s'\nxs[3] = %d\nxs[259] = %d\ntestUcharX(3) = %d\ntestUcharX(%d) = %d\nfptr(3) = %d\nfptr(259) = %d\n",
           xstr(CC), xs[a], xs[b], testUcharX(a), b, testUcharX((unsigned char)b), fptr(a), fptr(b));
}

//////////////////////////////////
// Tests for passing and returning Structs

// Complex-like data types
typedef struct {
    long real;
    long imag;
} complex_t;

complex_t ctest(complex_t a) {
    //Unpack a ComplexPair{Int} struct
    if (verbose) printf("%ld + %ld i\n", a.real, a.imag);
    a.real += 1;
    a.imag -= 2;
    return a;
}

complex double cgtest(complex double a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) printf("%g + %g i\n", creal(a), cimag(a));
    a += 1 - 2i;
    return a;
}

complex double* cgptest(complex double *a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) printf("%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - 2i;
    return a;
}

complex float cftest(complex float a) {
    //Unpack a ComplexPair{Float32} struct
    if (verbose) printf("%g + %g i\n", creal(a), cimag(a));
    a += 1 - 2i;
    return a;
}

complex float* cfptest(complex float *a) {
    //Unpack a ComplexPair{Float64} struct
    if (verbose) printf("%g + %g i\n", creal(*a), cimag(*a));
    *a += 1 - 2i;
    return a;
}

complex_t* cptest(complex_t *a) {
    //Unpack a ComplexPair{Int} struct pointer
    if (verbose) printf("%ld + %ld i\n", a->real, a->imag);
    a->real += 1;
    a->imag -= 2;
    return a;
}

// Native-like data types
char* stest(char *x) {
    //Print a character Array
    if (verbose) printf("%s\n", x);
    return x;
}
char* sptest(struct { struct { void* type; char* data; } *data; } str ) {
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
    long x;
    long y;
    char z;
} struct_big;

struct1 test_1(struct1 a) {
    //Unpack a "small" struct { float, double }
    if (verbose) printf("%g + %g i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

struct2a test_2a(struct2a a) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) printf("%" PRId32 " + %" PRId32 " i\n", a.x.x, a.y.y);
    a.x.x += 1;
    a.y.y -= 2;
    return a;
}

struct2b test_2b(struct2b a) {
    //Unpack a ComplexPair{Int32} struct
    if (verbose) printf("%" PRId32 " + %" PRId32 " i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

struct3a test_3a(struct3a a) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) printf("%" PRId64 " + %" PRId64 " i\n", a.x.x, a.y.y);
    a.x.x += 1;
    a.y.y -= 2;
    return a;
}

struct3b test_3b(struct3b a) {
    //Unpack a ComplexPair{Int64} struct
    if (verbose) printf("%" PRId64 " + %" PRId64 " i\n", a.x, a.y);
    a.x += 1;
    a.y -= 2;
    return a;
}

#define int128_t struct3b
int128_t test_4(int128_t a) {
    //Unpack a Int128
    if (verbose) printf("0x%016" PRIx64 "%016" PRIx64 "\n", a.y, a.x);
    a.x += 1;
    if (a.x == 0)
        a.y += 1;
    return a;
}

struct_big test_big(struct_big a) {
    //Unpack a "big" struct { int, int, char }
    if (verbose) printf("%ld %ld %c\n", a.x, a.y, a.z);
    a.x += 1;
    a.y -= 2;
    a.z -= 'A';
    return a;
}

//////////////////////////////////
// Turn off verbose for automated tests, leave on for debugging
void set_verbose(int level) {
    verbose = level;
}
